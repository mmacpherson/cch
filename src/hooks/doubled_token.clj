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

  PostToolUse cannot undo the edit — a block-decision surfaces the finding
  as conversation context so Claude can fix the identifier immediately."
  (:require [cch.core :refer [defhook]]
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

(defn check-file
  "Read file and check for doubled tokens. Returns nil or decision map."
  [file-path]
  (try
    (let [f (java.io.File. (str file-path))]
      (when (.isFile f)
        (when-let [hits (find-doubled-tokens (slurp f))]
          (let [summary (->> hits
                             (take 5)
                             (map #(str "  L" (:line %) ":" (:col %) " " (:match %)))
                             (str/join "\n"))]
            {:decision :block
             :reason   (str "doubled-token: " (count hits) " suspect identifier(s) in "
                            file-path ":\n" summary)}))))
    (catch java.io.FileNotFoundException _ nil)))

(defhook doubled-token
  "Detect doubled tokens after Edit — catches rename artifacts."
  {}
  [input]
  (when-let [file-path (get-in input [:tool_input :file_path])]
    (check-file file-path)))
