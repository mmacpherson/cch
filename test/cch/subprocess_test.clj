(ns cch.subprocess-test
  (:require [cch.subprocess :as subprocess]
            [clojure.test :refer [deftest is]]))

(deftest captures-stdout-stderr-and-exit-without-a-shell
  (let [result (subprocess/run
                 ["bash" "-c" "printf out; printf err >&2; exit 7"])]
    (is (= 7 (:exit result)))
    (is (= "out" (:out result)))
    (is (= "err" (:err result)))))

(deftest stdin-is-closed-so-a-prompting-command-sees-eof
  ;; `cat` with an open stdin pipe would block forever.
  (let [result (subprocess/run ["cat"] {:timeout-ms 5000})]
    (is (= 0 (:exit result)))
    (is (= "" (:out result)))))

(deftest a-command-that-overruns-its-timeout-is-killed
  (let [started (System/nanoTime)
        error (try
                (subprocess/run ["bash" "-c" "sleep 30"] {:timeout-ms 200})
                nil
                (catch clojure.lang.ExceptionInfo error error))
        elapsed-ms (/ (- (System/nanoTime) started) 1e6)]
    (is (= :subprocess-timeout (:type (ex-data error))))
    (is (< elapsed-ms 5000) "the wait is bounded by the timeout, not the child")))
