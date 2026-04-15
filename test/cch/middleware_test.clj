(ns cch.middleware-test
  (:require [clojure.test :refer [deftest is testing]]
            [cch.middleware :as mw]
            [cch.core :as core]
            [cch.log]))

(deftest test-wrap-timing
  (let [handler  (fn [_] {:decision :allow :reason "ok"})
        wrapped  (mw/wrap-timing handler)
        result   (wrapped {:tool_name "Edit"})]
    (is (= :allow (:decision result)))
    (is (number? (:cch/elapsed-ms (meta result))))
    (is (< (:cch/elapsed-ms (meta result)) 100))))

(deftest test-wrap-timing-nil-result
  (testing "timing preserves nil (allow) result"
    (let [handler (fn [_] nil)
          wrapped (mw/wrap-timing handler)
          result  (wrapped {})]
      (is (nil? result)))))

(deftest test-wrap-error-handler
  (testing "catches exceptions and returns deny"
    (let [handler (fn [_] (throw (ex-info "boom" {})))
          wrapped (mw/wrap-error-handler handler)
          result  (wrapped {})]
      (is (= :deny (:decision result)))
      (is (re-find #"boom" (:reason result))))))

(deftest test-wrap-error-handler-passthrough
  (testing "passes through normal results"
    (let [handler (fn [_] {:decision :ask :reason "hmm"})
          wrapped (mw/wrap-error-handler handler)
          result  (wrapped {})]
      (is (= :ask (:decision result))))))

(deftest test-compose-middleware
  (testing "middleware composes in correct order"
    (let [handler  (fn [_] {:decision :allow :reason "ok"})
          composed (core/compose-middleware handler mw/default-middleware)
          result   (composed {:tool_name "Edit"})]
      (is (= :allow (:decision result)))
      (is (number? (:cch/elapsed-ms (meta result)))))))

(deftest test-wrap-logging-non-blocking
  (testing "wrap-logging adds < 30ms p95 to the hot path on the per-call
            fallback path (no writer running). Regression guard: if this
            fails, the INSERT is no longer fire-and-forget — check
            ProcessBuilder.start behavior and that :out/:err are :discard."
    (let [handler (fn [_] {:decision :allow :reason "ok"})
          wrapped (mw/wrap-logging handler)
          input   {:hook_event_name "PreToolUse"
                   :tool_name       "Edit"
                   :tool_input      {:file_path "/tmp/t"}
                   :session_id      "test"
                   :cch/hook-name   "regression"}
          _ (dotimes [_ 5] (wrapped input))
          samples (vec (repeatedly 20
                         #(let [start (System/nanoTime)]
                            (wrapped input)
                            (/ (- (System/nanoTime) start) 1e6))))
          sorted  (vec (sort samples))
          p95     (nth sorted 18)]
      (is (< p95 30.0)
          (format "wrap-logging p95 = %.2fms (samples: %s)"
                  p95 (vec (map #(format "%.1f" %) sorted)))))))

(deftest test-wrap-logging-with-writer-is-fast
  (testing "with the background writer running, wrap-logging adds <2ms p95
            (queue offer + memory work — no per-call fork+exec)."
    (cch.log/start-writer!)
    (try
      (let [handler (fn [_] {:decision :allow :reason "ok"})
            wrapped (mw/wrap-logging handler)
            input   {:hook_event_name "PreToolUse"
                     :tool_name       "Edit"
                     :tool_input      {:file_path "/tmp/t"}
                     :session_id      "test"
                     :cch/hook-name   "regression-queued"}
            _ (dotimes [_ 10] (wrapped input))
            samples (vec (repeatedly 50
                           #(let [start (System/nanoTime)]
                              (wrapped input)
                              (/ (- (System/nanoTime) start) 1e6))))
            sorted  (vec (sort samples))
            p95     (nth sorted (int (* (count sorted) 0.95)))]
        (is (< p95 2.0)
            (format "queued wrap-logging p95 = %.3fms (samples: %s)"
                    p95 (vec (map #(format "%.2f" %) (take 10 sorted))))))
      (finally
        (cch.log/stop-writer!)))))
