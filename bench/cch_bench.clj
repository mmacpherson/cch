(ns cch-bench
  "Performance benchmarks for cch.

  Two flavors, each appropriate for what it measures:

  1. Pure-function microbenchmarks via Criterium. Hugo Duncan's library
     handles JIT warmup, GC noise, outliers, and proper statistics for
     anything you can call in a tight loop with no I/O.

  2. End-to-end HTTP dispatch via a sequential client loop. Criterium
     is wrong for this — it would mask the JIT-compilation pause that
     shows up as a cold-path p99 spike, and that spike is the most
     interesting number for a long-running server.

  Run all:    clj -M:bench
  Pure only:  clj -M:bench :only pure
  HTTP only:  clj -M:bench :only http  (requires server on :8888)

  Output is a markdown-formatted report on stdout. Pipe to a file in
  bench/reports/<date>.md to keep a record."
  (:require [babashka.fs :as fs]
            [cch.db :as db]
            [cch.projections :as proj]
            [cch.server :as server]
            [cheshire.core :as json]
            [criterium.core :as c]
            [hato.client :as http]
            [hooks.command-audit :as cmd-audit]
            [hooks.command-guard :as cmd-guard]
            [hooks.protect-files :as protect]
            [hooks.scope-lock :as scope])
  (:import (java.net ServerSocket)
           (java.time Instant)))

(defn- now-epoch [] (.getEpochSecond (Instant/now)))

;; ---------------------------------------------------------------------------
;; Synthetic inputs

(def ^:private observed-7d
  "84 points spanning ~7 days, climbing from 5% to 60% — realistic shape
  for the long window."
  (let [now (now-epoch)
        n   84]
    (->> (range n)
         (mapv (fn [i]
                 {:ts  (- now (* (- n i) 7200))
                  :pct (+ 5.0 (* 55.0 (/ (double i) (dec n))))})))))

(def ^:private observed-5h
  "20 points spanning ~5 hours."
  (let [now (now-epoch)
        n   20]
    (->> (range n)
         (mapv (fn [i]
                 {:ts  (- now (* (- n i) 900))
                  :pct (+ 10.0 (* 70.0 (/ (double i) (dec n))))})))))

(def ^:private window-info-7d
  {:now      (now-epoch)
   :resets-at (+ (now-epoch) (* 4 86400))
   :last-pct 60.0})

(def ^:private window-info-5h
  {:now       (now-epoch)
   :resets-at (+ (now-epoch) 3600)
   :last-pct  80.0})

(def ^:private audit-patterns
  ["rm -rf" "curl .* \\| sh" "chmod 777" "git push --force"])

;; ---------------------------------------------------------------------------
;; Pure microbenchmarks

(defn- format-time
  "Format a duration in seconds (Criterium native unit) as the most
  appropriate sub-second unit."
  [secs]
  (cond
    (>= secs 1)      (format "%.3f s"  secs)
    (>= secs 1e-3)   (format "%.3f ms" (* secs 1e3))
    (>= secs 1e-6)   (format "%.3f µs" (* secs 1e6))
    :else            (format "%.0f ns"  (* secs 1e9))))

(def ^:private pure-results (atom []))

(defn- log-progress [s]
  (binding [*out* *err*] (println s)))

(defn- bench-quick [label f]
  (log-progress (str "  " label "..."))
  (let [{:keys [mean variance]} (c/quick-benchmark (f) {})]
    (swap! pure-results conj
           {:label label
            :mean  (first mean)
            :sigma (Math/sqrt (first variance))})))

(defn- emit-pure-table []
  (println "\n| Function | Mean | σ |")
  (println "| --- | ---: | ---: |")
  (doseq [{:keys [label mean sigma]} @pure-results]
    (println (format "| `%s` | %s | %s |"
                     label (format-time mean) (format-time sigma)))))

(defn pure-bench []
  (reset! pure-results [])
  (log-progress "Running pure-function microbenchmarks (Criterium quick-bench).")
  (log-progress "Each takes ~36s; expect ~6 minutes total.\n")

  (bench-quick "rate-bayes-projection / 7-day window (84 pts)"
               #(proj/rate-bayes-projection observed-7d window-info-7d))

  (bench-quick "rate-bayes-projection / 5-hour window (20 pts)"
               #(proj/rate-bayes-projection observed-5h window-info-5h))

  (bench-quick "command-audit/check-command — no match"
               #(cmd-audit/check-command "ls -la /tmp" audit-patterns))

  (bench-quick "command-audit/check-command — first-pattern match"
               #(cmd-audit/check-command "rm -rf /tmp/scratch" audit-patterns))

  (bench-quick "command-guard/check-dangerous — safe command"
               #(cmd-guard/check-dangerous "git status" nil))

  (bench-quick "command-guard/check-dangerous — destructive"
               #(cmd-guard/check-dangerous "rm -rf /" nil))

  (bench-quick "protect-files/check-path — normal source file"
               #(protect/check-path "/repo/src/main.clj" protect/default-patterns))

  (bench-quick "protect-files/check-path — sensitive (.env)"
               #(protect/check-path "/repo/.env" protect/default-patterns))

  (bench-quick "scope-lock/check-scope — inside worktree"
               #(scope/check-scope "/repo/src/main.clj" "/repo" nil))

  (bench-quick "scope-lock/check-scope — outside worktree"
               #(scope/check-scope "/etc/passwd" "/repo" nil))

  (println "## Pure-function microbenchmarks")
  (println)
  (println "Run via Criterium's `quick-bench` (~6s warmup, ~30s measurement,")
  (println "outlier detection, statistical analysis). Times reported are")
  (println "per-call in steady-state, after JIT warmup.")
  (emit-pure-table))

;; ---------------------------------------------------------------------------
;; End-to-end HTTP dispatch

(def ^:private dispatch-payload
  (json/generate-string
    {:hook_event_name "PreToolUse"
     :tool_name       "Bash"
     :cwd             "/tmp/cch-bench"
     :tool_input      {:command "ls -la"}}))
(def ^:private headers {"Content-Type" "application/json"})

(defn- free-port []
  (with-open [s (ServerSocket. 0)]
    (.getLocalPort s)))

(defn- delete-tree-tolerantly
  "Avoid races with SQLite WAL cleanup that can unlink -wal between our
   directory listing and our delete call (NoSuchFileException on a file
   that's already gone is the desired end state)."
  [dir]
  (let [path (java.nio.file.Paths/get (str dir) (into-array String []))]
    (when (java.nio.file.Files/exists path (into-array java.nio.file.LinkOption []))
      (java.nio.file.Files/walkFileTree
        path
        (proxy [java.nio.file.SimpleFileVisitor] []
          (visitFile [file _attrs]
            (try (java.nio.file.Files/delete file)
                 (catch java.nio.file.NoSuchFileException _ nil))
            java.nio.file.FileVisitResult/CONTINUE)
          (postVisitDirectory [d _exc]
            (try (java.nio.file.Files/delete d)
                 (catch java.nio.file.NoSuchFileException _ nil))
            java.nio.file.FileVisitResult/CONTINUE))))))

(defn- with-ephemeral-server
  "Run f against a fresh in-process dispatcher backed by a tmp DB on a
  free port. Tears it all down on the way out so the bench never touches
  the production DB at ~/.local/share/cch/events.db."
  [f]
  (let [tmp-dir   (str (fs/create-temp-dir {:prefix "cch-bench-"}))
        tmp-db    (str tmp-dir "/events.db")
        port      (free-port)
        original  db/db-path]
    (alter-var-root #'db/db-path (constantly (constantly tmp-db)))
    (let [{:keys [stop]} (binding [*out* (java.io.StringWriter.)]
                           (server/start! {:port port :host "127.0.0.1"}))]
      (try
        (f port)
        (finally
          (stop)
          (alter-var-root #'db/db-path (constantly original))
          (delete-tree-tolerantly tmp-dir))))))

(defn- run-batch [url start end]
  (let [t0   (System/nanoTime)
        lats (long-array (- end start))]
    (loop [i start]
      (when (< i end)
        (let [a (System/nanoTime)]
          (http/post url
                     {:body dispatch-payload :headers headers
                      :throw-exceptions? false})
          (aset lats (- i start) (- (System/nanoTime) a))
          (recur (inc i)))))
    {:total-ns (- (System/nanoTime) t0) :lats lats}))

(defn- percentile [^longs lats p]
  (let [sorted (java.util.Arrays/copyOf lats (alength lats))
        _      (java.util.Arrays/sort sorted)
        idx    (min (dec (alength sorted))
                    (int (* p (alength sorted))))]
    (aget sorted idx)))

(defn- ms [ns] (format "%.3f" (/ ns 1e6)))

(defn- emit-http-row [label batch]
  (let [{:keys [lats]} batch
        n (alength ^longs lats)]
    (println (format "| %s | %d | %s | %s | %s | %s | %s |"
                     label n
                     (ms (/ (apply + (seq lats)) n))
                     (ms (percentile lats 0.50))
                     (ms (percentile lats 0.90))
                     (ms (percentile lats 0.99))
                     (ms (percentile lats 0.999))))))

(defn http-bench []
  (log-progress "Starting ephemeral dispatcher (tmp DB, free port)...")
  (with-ephemeral-server
    (fn [port]
      (let [url (str "http://127.0.0.1:" port "/dispatch/PreToolUse")]
        (log-progress (str "  → " url))
        (log-progress "Running 10,000 sequential POSTs (~30s)...")
        (println "\n## End-to-end HTTP dispatch")
        (println)
        (println "10,000 sequential POSTs to a freshly-spawned in-process")
        (println "dispatcher backed by a tmp SQLite DB. The cold→hot curve")
        (println "is HotSpot C2 compiling hot methods from interpreted bytecode")
        (println "to native. The dispatcher is torn down at the end so the")
        (println "production DB is never touched.")
        (println)
        (let [cold (run-batch url 0 100)
              mid  (run-batch url 100 1000)
              warm (run-batch url 1000 5000)
              hot  (run-batch url 5000 10000)]
          (println "| Phase | n | mean (ms) | p50 (ms) | p90 (ms) | p99 (ms) | p99.9 (ms) |")
          (println "| --- | ---: | ---: | ---: | ---: | ---: | ---: |")
          (emit-http-row "cold (0–100)"  cold)
          (emit-http-row "mid  (100–1k)" mid)
          (emit-http-row "warm (1k–5k)"  warm)
          (emit-http-row "hot  (5k–10k)" hot))))))

;; ---------------------------------------------------------------------------
;; Entry point

(defn -main [& args]
  (let [only (when (= ":only" (first args)) (second args))]
    (println "# cch performance report")
    (println)
    (println (format "- **Generated:** %s" (Instant/now)))
    (println (format "- **JVM:** %s %s"
                     (System/getProperty "java.vm.name")
                     (System/getProperty "java.version")))
    (println (format "- **OS:** %s %s"
                     (System/getProperty "os.name")
                     (System/getProperty "os.arch")))
    (println)
    (case only
      "pure" (pure-bench)
      "http" (http-bench)
      (do (pure-bench) (http-bench))))
  (System/exit 0))
