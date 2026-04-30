(ns cch.config-db-test
  (:require [babashka.fs :as fs]
            [cch.config-db :as cdb]
            [cch.db :as db]
            [clojure.test :refer [deftest is testing use-fixtures]]))

;; Each test gets a fresh temp DB; with-redefs swaps db-path so both
;; the code under test and the log ns that config-db relies on see
;; the same fake location.

(def ^:dynamic *tmp-db* nil)

(defn with-tmp-db [t]
  (let [tmp (str (fs/create-temp-dir {:prefix "cfg-db-test-"}))
        db  (str tmp "/events.db")]
    (try
      (binding [*tmp-db* db]
        (with-redefs [db/db-path (fn [] db)]
          (t)))
      (finally
        (fs/delete-tree tmp)))))

(use-fixtures :each with-tmp-db)

(deftest test-upsert-roundtrip
  (testing "insert a row, read it back"
    (cdb/upsert! {:hook-name "scope-lock"
                  :scope     cdb/global-scope
                  :enabled   true
                  :options   {:allowed-paths ["src/" ".claude/"]}})
    (let [row (cdb/get-row "scope-lock" cdb/global-scope)]
      (is (= "scope-lock" (:hook-name row)))
      (is (= "global" (:scope row)))
      (is (true? (:enabled row)))
      (is (= {:allowed-paths ["src/" ".claude/"]} (:options row))))))

(deftest test-upsert-overwrites
  (testing "upsert on existing PK updates enabled and options"
    (cdb/upsert! {:hook-name "protect-files"
                  :scope     cdb/global-scope
                  :enabled   true
                  :options   nil})
    (cdb/upsert! {:hook-name "protect-files"
                  :scope     cdb/global-scope
                  :enabled   false
                  :options   {:extra-patterns ["**/secret/**"]}})
    (let [row (cdb/get-row "protect-files" cdb/global-scope)]
      (is (false? (:enabled row)))
      (is (= {:extra-patterns ["**/secret/**"]} (:options row))))))

(deftest test-delete
  (testing "delete removes the row"
    (cdb/upsert! {:hook-name "scope-lock" :scope cdb/global-scope :enabled true})
    (is (some? (cdb/get-row "scope-lock" cdb/global-scope)))
    (cdb/delete! "scope-lock" cdb/global-scope)
    (is (nil? (cdb/get-row "scope-lock" cdb/global-scope))))

  (testing "delete on missing row is a no-op"
    (cdb/delete! "never-existed" cdb/global-scope)
    (is (nil? (cdb/get-row "never-existed" cdb/global-scope)))))

(deftest test-repo-scope-format
  (testing "repo-scope prefixes the path"
    (is (= "repo:/home/me/project" (cdb/repo-scope "/home/me/project")))))

(deftest test-list-all-orders
  (testing "list-all returns rows ordered by hook_name then scope"
    (cdb/upsert! {:hook-name "scope-lock"    :scope (cdb/repo-scope "/z") :enabled true})
    (cdb/upsert! {:hook-name "scope-lock"    :scope cdb/global-scope      :enabled true})
    (cdb/upsert! {:hook-name "protect-files" :scope cdb/global-scope      :enabled false})
    (let [rows (cdb/list-all)
          pairs (mapv (juxt :hook-name :scope) rows)]
      (is (= [["protect-files" "global"]
              ["scope-lock"    "global"]
              ["scope-lock"    "repo:/z"]]
             pairs)))))

(deftest test-list-for-scope
  (testing "list-for-scope filters"
    (cdb/upsert! {:hook-name "a" :scope cdb/global-scope      :enabled true})
    (cdb/upsert! {:hook-name "a" :scope (cdb/repo-scope "/x") :enabled false})
    (cdb/upsert! {:hook-name "b" :scope (cdb/repo-scope "/x") :enabled true})
    (let [rows (cdb/list-for-scope (cdb/repo-scope "/x"))]
      (is (= 2 (count rows)))
      (is (= #{"a" "b"} (set (map :hook-name rows)))))))

(deftest test-options-nil-roundtrip
  (testing "nil options stays nil, not empty map"
    (cdb/upsert! {:hook-name "x" :scope cdb/global-scope :enabled true :options nil})
    (let [row (cdb/get-row "x" cdb/global-scope)]
      (is (nil? (:options row))))))

(deftest test-sql-escaping
  (testing "single-quotes in values don't break SQL"
    (cdb/upsert! {:hook-name "it's-a-hook"
                  :scope     cdb/global-scope
                  :enabled   true
                  :options   {:note "can't stop"}})
    (let [row (cdb/get-row "it's-a-hook" cdb/global-scope)]
      (is (= "it's-a-hook" (:hook-name row)))
      (is (= {:note "can't stop"} (:options row))))))
