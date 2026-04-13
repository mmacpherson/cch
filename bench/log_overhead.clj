(ns log-overhead
  "Benchmark the overhead wrap-logging adds to the hot path.

  Run with:
    bb -cp src:resources:bench -m log-overhead

  Measures:
    - warm-DB p50/p95/p99 overhead of wrap-logging vs. a bare no-op handler
    - cold-DB cost (DB path does not yet exist)
    - CCH_LOG_SYNC=1 cost for comparison"
  (:require [cch.log :as log]
            [cch.middleware :as mw]
            [babashka.fs :as fs]))

(defn- percentile [sorted p]
  (let [idx (int (* p (count sorted)))]
    (nth sorted (min idx (dec (count sorted))))))

(defn- stats [samples]
  (let [sorted (vec (sort samples))]
    {:n    (count sorted)
     :min  (first sorted)
     :p50  (percentile sorted 0.50)
     :p95  (percentile sorted 0.95)
     :p99  (percentile sorted 0.99)
     :max  (last sorted)
     :mean (/ (reduce + samples) (count samples))}))

(defn- time-ns [f]
  (let [start (System/nanoTime)]
    (f)
    (- (System/nanoTime) start)))

(defn- ns->ms [ns] (/ ns 1e6))

(defn- bench [label n thunk]
  ;; warmup
  (dotimes [_ 20] (thunk))
  (let [samples (vec (repeatedly n #(time-ns thunk)))
        s       (stats samples)]
    (println (format "%-30s n=%d  p50=%.3fms  p95=%.3fms  p99=%.3fms  max=%.3fms  mean=%.3fms"
                     label
                     (:n s)
                     (ns->ms (:p50 s))
                     (ns->ms (:p95 s))
                     (ns->ms (:p99 s))
                     (ns->ms (:max s))
                     (ns->ms (:mean s))))
    s))

(def ^:private no-op (fn [_] {:decision :allow :reason "ok"}))

(def ^:private sample-input
  {:hook_event_name "PreToolUse"
   :tool_name       "Edit"
   :tool_input      {:file_path "/tmp/bench.txt"}
   :session_id      "bench-session"
   :cwd             "/tmp"
   :cch/hook-name   "bench"})

(defn -main [& _]
  (println "=== wrap-logging hot-path overhead ===\n")
  (println (str "DB path: " (log/db-path)))
  (println (str "DB exists: " (fs/exists? (log/db-path))))
  (println)

  ;; Ensure warm DB for the main measurement
  (log/ensure-db! (log/db-path))

  (println "Warm DB (async fire-and-forget):")
  (bench "bare no-op handler"    1000 #(no-op sample-input))
  (bench "wrap-timing only"      1000 #((mw/wrap-timing no-op) sample-input))
  (let [wrapped (mw/wrap-logging no-op)]
    (bench "wrap-logging (reused)" 1000 #(wrapped sample-input)))
  (let [composed (reduce (fn [h wrap] (wrap h))
                         no-op
                         (reverse mw/default-middleware))]
    (bench "full stack (reused)"   1000 #(composed sample-input)))

  (println "\nIsolating the cost within wrap-logging:")
  (bench "ensure-db! (warm)"     1000 #(log/ensure-db! (log/db-path)))
  (bench "p/process sqlite3"     100
         #(babashka.process/process ["sqlite3" (log/db-path) "SELECT 1;"]
                                    {:out :discard :err :discard}))

  (println "\nCold DB (first-call ensure-db! path):")
  (let [tmp (str (fs/create-temp-dir {:prefix "bench-cold-"}))
        tmp-db (str tmp "/events.db")]
    (try
      ;; Override db-path via env
      (System/setProperty "user.home" tmp)
      (let [orig-path (log/db-path)]
        (println (str "  (simulating via tmp dir: " tmp ")"))
        ;; Not a true cold start — JVM is warm — but captures ensure-db! cost
        (bench "cold ensure-db! + log" 5
               #(do (fs/delete-if-exists tmp-db)
                    ((mw/wrap-logging no-op) sample-input))))
      (finally
        (fs/delete-tree tmp))))

  (println "\nSync mode (CCH_LOG_SYNC=1 — sanity check, NOT hot path):")
  (println "  Skipped — requires env var at process start.")
  (println "  Run manually: CCH_LOG_SYNC=1 bb -cp src:resources:bench -m log-overhead")

  (println "\nDone."))
