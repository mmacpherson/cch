(ns hooks.codex-usage-capture
  "Captures Codex rate-limit data from session rollouts and writes it to
  context_snapshots — the same table Claude Code's statusLine feeds. Lets
  the existing forecast/usage code render the Codex tab without a parallel
  data pipeline.

  Why a hook instead of a statusLine command: Codex 0.133.0 doesn't expose
  a Claude-style statusLine.command. Its rate-limit snapshot is appended
  to $CODEX_HOME/sessions/.../rollout-*.jsonl as `event_msg`/`token_count`
  events. Every Codex hook receives `transcript_path` pointing at that
  file, so the freshest rate-limit row is one tail-read away.

  Runs on Stop (once per turn-end). For non-Codex agents this is a no-op."
  (:require [babashka.fs :as fs]
            [cch.core :refer [defhook]]
            [cch.log :as log]
            [cheshire.core :as json]
            [clojure.string :as str])
  (:import [java.io ByteArrayOutputStream RandomAccessFile]
           [java.nio.charset StandardCharsets]))

(def ^:private claude-window-by-minutes
  "Map codex's `window_minutes` onto Claude Code's rate_limits key. Match
  by minutes rather than by primary/secondary position to survive any
  schema reordering by codex."
  {300   "five_hour"
   10080 "seven_day"})

(defn parse-jsonl-lines
  "Parse JSONL content (string) into a vector of maps. Skips lines that
  fail to parse so one corrupt line doesn't poison the result. Pure."
  [s]
  (->> (clojure.string/split-lines (or s ""))
       (keep (fn [line]
               (when-not (clojure.string/blank? line)
                 (try (json/parse-string line true)
                      (catch Exception _ nil)))))
       vec))

(defn latest-token-count
  "Most recent `event_msg`/`token_count` entry in a Codex rollout JSONL,
  or nil if none. Pure."
  [parsed-lines]
  (->> parsed-lines
       (filter #(and (= "event_msg" (:type %))
                     (= "token_count" (get-in % [:payload :type]))))
       last))

(def ^:private reverse-scan-chunk-bytes (* 64 1024))
(def ^:private max-jsonl-line-bytes (* 1024 1024))

(defn- reverse-bytes!
  [^bytes bytes]
  (loop [left 0
         right (dec (alength bytes))]
    (when (< left right)
      (let [value (aget bytes left)]
        (aset-byte bytes left (aget bytes right))
        (aset-byte bytes right value)
        (recur (inc left) (dec right)))))
  bytes)

(defn- token-count-line
  "Parse a reverse-buffered JSONL line only when it could be a token-count
  event. The string prefilter avoids constructing full JSON trees for the
  overwhelmingly more common transcript records."
  [^ByteArrayOutputStream reversed-line]
  (when (pos? (.size reversed-line))
    (let [line (String. ^bytes (reverse-bytes! (.toByteArray reversed-line))
                        StandardCharsets/UTF_8)]
      (when (and (str/includes? line "\"event_msg\"")
                 (str/includes? line "\"token_count\""))
        (try
          (let [parsed (json/parse-string line true)]
            (when (and (= "event_msg" (:type parsed))
                       (= "token_count" (get-in parsed [:payload :type])))
              parsed))
          (catch Exception _ nil))))))

(defn latest-token-count-in-file
  "Find the newest valid token-count record by scanning a rollout backward.

  Memory is bounded by one fixed read chunk plus at most 1 MiB for a JSONL
  record. Oversized and malformed records are skipped. Taking the file length
  once also makes a concurrently appended partial final record harmless: it is
  skipped and the previous complete token-count record is returned."
  [path]
  (when (and path (fs/exists? path))
    (try
      (with-open [file (RandomAccessFile. (str path) "r")]
        (loop [next-position (.length file)
               chunk (byte-array 0)
               index -1
               reversed-line (ByteArrayOutputStream.)
               oversized? false]
          (cond
            (>= index 0)
            (let [value (aget ^bytes chunk index)]
              (if (= 10 (bit-and value 0xff))
                (if-let [match (when-not oversized?
                                 (token-count-line reversed-line))]
                  match
                  (recur next-position chunk (dec index)
                         (ByteArrayOutputStream.) false))
                (let [store? (and (not oversized?)
                                  (< (.size reversed-line)
                                     max-jsonl-line-bytes))]
                  (when store?
                    (.write reversed-line (bit-and value 0xff)))
                  (recur next-position chunk (dec index) reversed-line
                         (or oversized? (not store?))))))

            (pos? next-position)
            (let [start (max 0 (- next-position reverse-scan-chunk-bytes))
                  size (int (- next-position start))
                  bytes (byte-array size)]
              (.seek file start)
              (.readFully file bytes)
              (recur start bytes (dec size) reversed-line oversized?))

            :else
            (when-not oversized?
              (token-count-line reversed-line)))))
      (catch Exception _ nil))))

(defn codex->claude-rate-limits
  "Convert codex's `rate_limits` map (primary/secondary keyed by
  window_minutes) into Claude's (five_hour/seven_day). Drops windows
  at 0% — they carry no information and their rolling resets_at
  poisons window selection in forecast queries. Pure."
  [codex-rl]
  (->> [(:primary codex-rl) (:secondary codex-rl)]
       (keep (fn [w]
               (when-let [k (claude-window-by-minutes (:window_minutes w))]
                 (when (and (:used_percent w) (pos? (:used_percent w)))
                   [k {:used_percentage (:used_percent w)
                       :resets_at       (:resets_at w)}]))))
       (into {})))

(defn build-snapshot
  "Pure: codex hook input + parsed rollout → log-context-snapshot! kwargs,
  or nil when there is nothing to log."
  [input parsed-lines]
  (when-let [tc (latest-token-count parsed-lines)]
    (let [rl     (get-in tc [:payload :rate_limits])
          claude {:session_id  (:session_id input)
                  :model       {:id (:model input)}
                  :rate_limits (codex->claude-rate-limits rl)}]
      (when (seq (:rate_limits claude))
        {:session-id (:session_id input)
         :model-id   (:model input)
         :payload    (json/generate-string claude)
         :agent      "codex"}))))

(defhook codex-usage-capture
  "Mirrors Codex rate-limit snapshots into context_snapshots so the /usage
  page's Codex tab uses the same projection pipeline as Claude."
  {}
  [input]
  (when (= "codex" (:cch/agent input))
    (when-let [token-count
               (latest-token-count-in-file (:transcript_path input))]
      (when-let [args (build-snapshot input [token-count])]
        (log/log-context-snapshot! args))))
  nil)
