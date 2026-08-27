(ns hooks.doubled-token
  "PostToolUse hook for Edit: detect doubled tokens in modified files.

  Catches rename artifacts that produce syntactically valid but obviously
  wrong identifiers — the #1 uncaught bug class that linters miss.

  Examples:
    replace_all old='latest' new='observations_latest'
    → pre-existing 'observations_latest' becomes 'observations_observations_latest'

    replace_all old='get' new='get_b2'
    → pre-existing 'get_b2_client' becomes 'get_b2_b2_client'

  Two detection strategies:
    1. Identifier split: extract identifiers, split by _ or -, check for
       repeated consecutive parts at any subsequence length
    2. Regex: concatenated doubles without separator (getget, foofoo)

  Only identifiers the edit INTRODUCES are reported — new-string is
  compared against old-string. Scanning the whole file instead means one
  pre-existing match blocks every later edit to that file forever; see
  check-edit for what that cost in practice.

  PostToolUse cannot undo the edit — a block-decision surfaces the finding
  as conversation context so Claude can fix the identifier immediately."
  (:require [cch.core :refer [defhook]]
            [clojure.set]
            [clojure.string :as str]))

;; Catches concatenated doubles without separator: getget, foofoo
;; Uses standard \b which works here because the capture excludes _
(def ^:private concat-doubled-re
  #"\b([a-zA-Z][a-zA-Z0-9]{2,})\1")

;; Extracts identifiers long enough to contain a doubled part (min 5 chars: xx_xx)
;; Includes - for kebab-case (CSS classes, Clojure symbols)
(def ^:private ident-re
  #"[a-zA-Z_][\w-]{4,}")

(defn- repeated-parts?
  "True if parts has consecutive repeated subsequences of any length k >= 1.
   [\"latest\" \"latest\"]                     → true (k=1)
   [\"observations\" \"latest\" \"latest\"]     → true (k=1 at index 1)
   [\"get\" \"b2\" \"get\" \"b2\" \"client\"]   → true (k=2 at index 0)
   [\"foo\" \"bar\" \"baz\"]                    → false"
  [parts]
  (let [pv (vec parts)
        n  (count pv)]
    (boolean
      (some (fn [k]
              (some (fn [i]
                      (= (subvec pv i (+ i k))
                         (subvec pv (+ i k) (+ i k k))))
                    (range 0 (inc (- n (* 2 k))))))
            (range 1 (inc (quot n 2)))))))

(defn- find-ident-hits
  "Extract identifiers from a line, split by _ or -, check for repeated parts."
  [line-num line]
  (let [m (re-matcher ident-re line)]
    (loop [hits []]
      (if (.find m)
        (let [ident (.group m)
              parts (str/split ident #"[-_]+")]
          (recur (if (and (>= (count parts) 2)
                          (repeated-parts? parts))
                   (conj hits {:line  line-num
                               :col   (.start m)
                               :match ident})
                   hits)))
        hits))))

(defn- find-concat-hits
  "Find concatenated doubled tokens (getget, foofoo) via regex."
  [line-num line]
  (let [m (re-matcher concat-doubled-re line)]
    (loop [hits []]
      (if (.find m)
        (recur (conj hits {:line  line-num
                           :col   (.start m)
                           :match (.group m)
                           :token (.group m 1)}))
        hits))))

(defn find-doubled-tokens
  "Scan text for doubled tokens. Returns seq of hit maps or nil.
   Pure function — no I/O."
  [text]
  (when-not (str/blank? text)
    (let [lines (str/split-lines text)]
      (seq
        (mapcat (fn [[idx line]]
                  (let [line-num (inc idx)]
                    (concat (find-ident-hits line-num line)
                            (find-concat-hits line-num line))))
                (map-indexed vector lines))))))

(defn- matches
  "Set of suspect identifiers in `s`, ignoring position. Position is
  deliberately dropped: the same identifier moves line and column as an
  edit shifts text around it, and comparing positions would report every
  survivor as new."
  [s]
  (into #{} (map :match) (or (find-doubled-tokens (or s "")) [])))

(defn check-edit
  "Flag only identifiers the edit INTRODUCES. Returns nil or a decision.

  Scanning the whole file — which this hook did until 2026-08-16 — means
  one pre-existing match blocks every later edit to that file, forever,
  whatever the edit changes. Measured over 741 real blocks across 103
  files: every one was followed by a byte-identical retry that
  succeeded, so nothing was being prevented and each block cost a turn.
  The worst example was a date, `Release-3-3-26`, blocking 42 edits to a
  document it had nothing to do with.

  Comparing new against old restores the original intent — catching a
  rename artifact at the moment it is written — while making anything
  already in the file irrelevant. Pure."
  [file-path old-string new-string]
  (let [introduced (clojure.set/difference (matches new-string)
                                           (matches old-string))]
    (when (seq introduced)
      {:decision :block
       :reason   (str "doubled-token: " (count introduced)
                      " suspect identifier(s) introduced in " file-path ":\n"
                      (->> introduced sort (take 5)
                           (map #(str "  " %))
                           (str/join "\n")))})))

(defhook doubled-token
  "Detect doubled tokens introduced by an Edit — catches rename artifacts."
  {}
  [input]
  (let [{:keys [file_path old_string new_string]} (:tool_input input)]
    (when file_path
      (check-edit file_path old_string new_string))))
