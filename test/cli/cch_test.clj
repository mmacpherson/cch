(ns cli.cch-test
  "The data-driven CLI dispatcher: parse options per the command's spec, route
  to its handler, and derive help/usage from the table."
  (:require [clojure.test :refer [deftest is testing]]
            [cli.cch :as cch]
            [cli.log-cmd :as log-cmd]
            [cch.doctor :as doctor]))

(deftest dispatch-parses-options-and-routes-to-handler
  (testing "options are parsed via the spec (with coercion) and passed through"
    (let [captured (atom nil)]
      ;; :fn entries are vars, so with-redefs on the handler is seen by dispatch.
      (with-redefs [log-cmd/run (fn [options _args] (reset! captured options))]
        (cch/-main "log" "--limit" "5" "--hook" "command-guard")
        (is (= 5 (:limit @captured)) "--limit coerced to a long via :parse-fn")
        (is (= "command-guard" (:hook @captured)))))))

(deftest positional-args-reach-the-handler
  (let [captured (atom nil)]
    (with-redefs [doctor/run (fn [_opts arguments] (reset! captured arguments))]
      (cch/-main "doctor" "extra1" "extra2")
      (is (= ["extra1" "extra2"] @captured)))))

(deftest unknown-command-prints-usage
  (let [out (with-out-str (cch/-main "bogus"))]
    (is (re-find #"Usage: cch" out))
    (is (re-find #"install" out) "usage lists commands straight from the table")
    (is (re-find #"doctor" out))
    (is (re-find #"control" out))))

(deftest no-command-prints-usage
  (let [out (with-out-str (cch/-main))]
    (is (re-find #"Usage: cch" out))))

(deftest help-flag-prints-per-command-summary
  (let [out (with-out-str (cch/-main "doctor" "--help"))]
    (is (re-find #"doctor" out))
    (is (re-find #"--cwd" out) "the summary is generated from the command's opt spec")))
