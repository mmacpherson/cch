(ns cch.control.naming
  "Stable, privacy-safe presentation names for opaque control-plane routes.

  Mnemonics are derived only from route ids. Provider-advertised names and
  explicit aliases are bounded broker-visible metadata and never become
  routing authority."
  (:require [clojure.string :as str])
  (:import [java.nio.charset StandardCharsets]
           [java.security MessageDigest]))

(def ^:const max-alias-length 48)
(def ^:const max-native-name-length 96)

(def ^:private unsafe-name-pattern
  #"[\p{Cc}\p{Cf}\p{Cs}\p{Co}\p{Cn}\p{Zl}\p{Zp}]")

(def ^:private adjectives
  ["amber" "brisk" "calm" "clear" "coral" "crisp" "dapper" "eager"
   "gentle" "golden" "jolly" "kind" "lively" "lucid" "merry" "nimble"
   "quiet" "rapid" "ready" "silver" "steady" "sunny" "swift" "tidy"
   "vivid" "warm" "witty" "young" "bright" "cool" "fair" "spry"])

(def ^:private nouns
  ["badger" "cedar" "comet" "dolphin" "falcon" "finch" "fox" "heron"
   "juniper" "kite" "lark" "lynx" "maple" "marten" "otter" "owl"
   "panda" "pine" "quail" "raven" "robin" "salmon" "seal" "sparrow"
   "tiger" "trout" "violet" "willow" "wolf" "wren" "yak" "zinnia"])

(defn normalize-alias
  "Normalize a user-chosen alias, returning nil to clear it. Reject controls,
  bidi/format characters, line separators, and overlong values."
  [value]
  (when-not (or (nil? value) (string? value))
    (throw (ex-info "Session alias must be text"
                    {:type :invalid-alias})))
  (let [alias (some-> value str/trim)]
    (when-not (str/blank? alias)
      (when (or (> (count alias) max-alias-length)
                (re-find unsafe-name-pattern alias))
        (throw (ex-info
                 (str "Session alias must be at most " max-alias-length
                      " characters without control or formatting characters")
                 {:type :invalid-alias
                  :max-length max-alias-length})))
      alias)))

(defn normalize-native-name
  "Normalize a provider-advertised session name. Blank names are absent;
  malformed, formatted, or overlong names are rejected at the broker boundary."
  [value]
  (when-not (or (nil? value) (string? value))
    (throw (ex-info "Native session name must be text"
                    {:type :invalid-native-name})))
  (let [native-name (some-> value str/trim)]
    (when-not (str/blank? native-name)
      (when (or (> (count native-name) max-native-name-length)
                (re-find unsafe-name-pattern native-name))
        (throw (ex-info
                 (str "Native session name must be at most "
                      max-native-name-length
                      " characters without control or formatting characters")
                 {:type :invalid-native-name
                  :max-length max-native-name-length})))
      native-name)))

(def ^:const max-auto-name-length 32)
(def ^:const max-summary-length 100)

(defn normalize-auto-name
  "A runner's model-written role name: lowercase kebab-case, at most 32
  characters. Anything else is dropped rather than rejected, so one bad
  summary never blocks a runner's presence."
  [value]
  (when (and (string? value)
             (<= 1 (count value) max-auto-name-length)
             (re-matches #"[a-z0-9]+(?:-[a-z0-9]+)*" value))
    value))

(defn normalize-summary
  "A runner's model-written one-line status, already scrubbed on the runner.
  Whitespace collapses to single spaces; overlong, control, or formatting
  characters drop it."
  [value]
  (when (string? value)
    (let [line (str/trim (str/replace value #"\s+" " "))]
      (when (and (not (str/blank? line))
                 (<= (count line) max-summary-length)
                 (not (re-find unsafe-name-pattern line)))
        line))))

(defn mnemonic
  "Derive a stable human-readable name from an opaque route id only. The
  suffix makes accidental word-pair collisions visible."
  [route-id]
  (let [digest (.digest (MessageDigest/getInstance "SHA-256")
                        (.getBytes (str route-id) StandardCharsets/UTF_8))
        unsigned #(bit-and 0xff (aget digest %))]
    (str (nth adjectives (mod (unsigned 0) (count adjectives))) "-"
         (nth nouns (mod (unsigned 1) (count nouns))) "-"
         (format "%02x%02x" (unsigned 2) (unsigned 3)))))

(defn present-session
  "Add presentation-only naming fields. Explicit aliases take precedence over
  provider-advertised names, and those over the runner's model-written auto
  name; all retain the mnemonic for disambiguation."
  [session]
  (let [fallback (mnemonic (:id session))
        alias (try
                (normalize-alias (:alias session))
                (catch clojure.lang.ExceptionInfo _ nil))
        native-name (try
                      (normalize-native-name (:name session))
                      (catch clojure.lang.ExceptionInfo _ nil))
        auto-name (normalize-auto-name (:auto-name session))
        preferred (or alias native-name auto-name)]
    (cond-> (assoc (dissoc session :alias :name)
                   :mnemonic fallback
                   :display-name (if preferred
                                   (str preferred " · " fallback)
                                   fallback))
      native-name (assoc :name native-name)
      alias (assoc :alias alias))))
