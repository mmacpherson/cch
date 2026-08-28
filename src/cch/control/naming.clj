(ns cch.control.naming
  "Stable, privacy-safe presentation names for opaque control-plane routes.

  Mnemonics are derived only from route ids. Explicit aliases are bounded
  broker-visible metadata and never become routing authority."
  (:require [clojure.string :as str])
  (:import [java.nio.charset StandardCharsets]
           [java.security MessageDigest]))

(def ^:const max-alias-length 48)

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
                (re-find #"[\p{Cc}\p{Cf}\p{Cs}\p{Co}\p{Cn}\p{Zl}\p{Zp}]"
                         alias))
        (throw (ex-info
                 (str "Session alias must be at most " max-alias-length
                      " characters without control or formatting characters")
                 {:type :invalid-alias
                  :max-length max-alias-length})))
      alias)))

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
  "Add presentation-only naming fields. The route id remains unchanged and
  aliases always retain the deterministic mnemonic for disambiguation."
  [session]
  (let [fallback (mnemonic (:id session))
        alias (try
                (normalize-alias (:alias session))
                (catch clojure.lang.ExceptionInfo _ nil))]
    (cond-> (assoc session
                   :mnemonic fallback
                   :display-name (if alias
                                   (str alias " · " fallback)
                                   fallback))
      alias (assoc :alias alias))))
