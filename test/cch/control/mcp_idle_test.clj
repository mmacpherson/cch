(ns cch.control.mcp-idle-test
  (:require [cch.control.mcp-idle :as idle]
            [clojure.test :refer [deftest is testing]]
            [plumcp.core.protocol :as p]))

(deftest parse-timeout-ms-resolves-env
  (testing "blank or absent yields the default"
    (is (= idle/default-timeout-ms (idle/parse-timeout-ms nil)))
    (is (= idle/default-timeout-ms (idle/parse-timeout-ms "")))
    (is (= idle/default-timeout-ms (idle/parse-timeout-ms "   "))))
  (testing "a positive integer is honored (whitespace tolerated)"
    (is (= 1000 (idle/parse-timeout-ms "1000")))
    (is (= 1000 (idle/parse-timeout-ms " 1000 "))))
  (testing "non-positive or unparseable disables the watchdog"
    (is (nil? (idle/parse-timeout-ms "0")))
    (is (nil? (idle/parse-timeout-ms "-5")))
    (is (nil? (idle/parse-timeout-ms "off")))
    (is (nil? (idle/parse-timeout-ms "12x")))))

(deftest idle-expired?-compares-elapsed-to-timeout
  (is (false? (idle/idle-expired? 1000 5000 5999)) "under the window")
  (is (true? (idle/idle-expired? 1000 5000 6000)) "exactly at the window")
  (is (true? (idle/idle-expired? 1000 5000 9999)) "well past the window")
  (is (false? (idle/idle-expired? nil 0 Long/MAX_VALUE)) "disabled never expires"))

(deftest start-watchdog!-disabled-returns-nil-and-never-fires
  (let [fired (atom false)]
    (is (nil? (idle/start-watchdog! {:activity (atom 0)
                                     :timeout-ms nil
                                     :now-fn (constantly Long/MAX_VALUE)
                                     :sleep-fn (fn [_] nil)
                                     :on-idle #(reset! fired true)})))
    (is (false? @fired))))

(deftest start-watchdog!-fires-on-idle-when-window-elapses
  (let [fired (promise)
        activity (atom 0)]
    (idle/start-watchdog! {:activity activity
                           :timeout-ms 1000
                           ;; Clock already past the window -> expires on first check.
                           :now-fn (constantly 10000)
                           :sleep-fn (fn [_] nil)
                           :on-idle #(deliver fired :fired)})
    (is (= :fired (deref fired 2000 :timed-out)))))

(deftest start-watchdog!-waits-while-activity-keeps-advancing
  ;; now stays within the window relative to activity for the first few polls,
  ;; then activity goes stale and the watchdog fires exactly once.
  (let [fired (atom 0)
        activity (atom 0)
        ticks (atom 0)
        t (idle/start-watchdog!
            {:activity activity
             :timeout-ms 1000
             :now-fn (fn [] (let [n (swap! ticks inc)]
                              ;; keep activity fresh for 3 checks, then let it lapse
                              (when (<= n 3) (reset! activity (* n 100)))
                              (* n 100)))
             :sleep-fn (fn [_] nil)
             :on-idle #(swap! fired inc)})]
    (.join ^Thread t 2000)
    (is (= 1 @fired) "on-idle runs once and the loop stops")))

(deftest activity-logger-stamps-on-inbound-and-delegates
  (let [calls (atom [])
        touched (atom 0)
        inner (reify p/ITrafficLogger
                (log-http-request [_ x] (swap! calls conj [:http-request x]))
                (log-http-response [_ x] (swap! calls conj [:http-response x]))
                (log-http-failure [_ x] (swap! calls conj [:http-failure x]))
                (log-incoming-jsonrpc-request [_ x] (swap! calls conj [:in-req x]))
                (log-outgoing-jsonrpc-request [_ x] (swap! calls conj [:out-req x]))
                (log-incoming-jsonrpc-success [_ id r] (swap! calls conj [:in-ok id r]))
                (log-outgoing-jsonrpc-success [_ id r] (swap! calls conj [:out-ok id r]))
                (log-incoming-jsonrpc-failure [_ id e] (swap! calls conj [:in-err id e]))
                (log-outgoing-jsonrpc-failure [_ id e] (swap! calls conj [:out-err id e]))
                (log-incoming-jsonrpc-notification [_ x] (swap! calls conj [:in-note x]))
                (log-outgoing-jsonrpc-notification [_ x] (swap! calls conj [:out-note x]))
                (log-mcpcall-failure [_ x] (swap! calls conj [:call-fail x]))
                (log-mcp-sse-message [_ x] (swap! calls conj [:sse x])))
        wrapped (idle/activity-logger inner #(swap! touched inc))]
    (testing "inbound client traffic stamps activity"
      (p/log-incoming-jsonrpc-request wrapped {:m 1})
      (p/log-incoming-jsonrpc-notification wrapped {:m 2})
      (p/log-incoming-jsonrpc-success wrapped "id" {:r 1})
      (p/log-incoming-jsonrpc-failure wrapped "id" {:e 1})
      (is (= 4 @touched)))
    (testing "purely outgoing traffic does not stamp activity"
      (p/log-outgoing-jsonrpc-request wrapped {:m 3})
      (p/log-outgoing-jsonrpc-notification wrapped {:m 4})
      (p/log-mcp-sse-message wrapped {:s 1})
      (is (= 4 @touched)))
    (testing "every call is delegated to the inner logger"
      (is (= [:in-req :in-note :in-ok :in-err :out-req :out-note :sse]
             (mapv first @calls))))))
