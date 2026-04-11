(ns cch.middleware-test
  (:require [clojure.test :refer [deftest is testing]]
            [cch.middleware :as mw]
            [cch.core :as core]))

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
