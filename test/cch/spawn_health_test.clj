(ns cch.spawn-health-test
  (:require [cch.spawn-health :as spawn-health]
            [clojure.test :refer [deftest is testing]]))

(deftest consecutive-spawn-failures-make-the-jvm-unhealthy
  (let [s0 {:consecutive-failures 0}
        fail #(spawn-health/next-state % "IOException" 3)]
    (is (not (:unhealthy? (-> s0 fail fail))))
    (is (:unhealthy? (-> s0 fail fail fail)))
    (is (= "IOException" (:last-error (fail s0))))
    (testing "one success resets the count"
      (is (= 0 (:consecutive-failures
                 (spawn-health/next-state (-> s0 fail fail) :ok 3)))))))

(deftest probe-reports-only-spawn-errors
  (is (= :ok (spawn-health/probe ["/bin/sh" "-c" "exit 0"])))
  (is (= :ok (spawn-health/probe ["/bin/sh" "-c" "exit 3"]))
      "a child's own exit status says nothing about spawning")
  (is (= "IOException" (spawn-health/probe ["/nonexistent/cch-probe"])))
  (is (= "ProbeTimeout" (spawn-health/probe ["/bin/sh" "-c" "sleep 5"] 100))
      "a child that never exits counts against the JVM"))

(deftest health-reports-ok-by-default
  (is (true? (:ok (spawn-health/health)))))
