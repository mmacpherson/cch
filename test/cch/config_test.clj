(ns cch.config-test
  (:require [clojure.test :refer [deftest is testing]]
            [cch.config :as config]
            [babashka.fs :as fs]))

(deftest test-find-config-up
  (let [tmp-dir (str (fs/create-temp-dir {:prefix "config-test-"}))
        sub-dir (str tmp-dir "/a/b/c")]
    (try
      (fs/create-dirs sub-dir)
      (spit (str tmp-dir "/.claude-hooks.edn") "{:log {:enabled true}}")

      (testing "finds config walking up from subdirectory"
        (is (= (str tmp-dir "/.claude-hooks.edn")
               (config/find-config-up sub-dir ".claude-hooks.edn"))))

      (testing "returns nil when config not found"
        (let [isolated (str (fs/create-temp-dir {:prefix "no-config-"}))]
          (try
            (is (nil? (config/find-config-up isolated ".nonexistent.edn")))
            (finally
              (fs/delete-tree isolated)))))
      (finally
        (fs/delete-tree tmp-dir)))))

(deftest test-find-config-up-with-boundary
  (let [tmp-dir  (str (fs/create-temp-dir {:prefix "boundary-test-"}))
        sub-dir  (str tmp-dir "/project/src/deep")
        boundary (str tmp-dir "/project")]
    (try
      (fs/create-dirs sub-dir)

      (testing "finds config within boundary"
        (spit (str tmp-dir "/project/.scope-lock.edn") "{:allowed-paths [\"src/\"]}")
        (is (= (str tmp-dir "/project/.scope-lock.edn")
               (config/find-config-up sub-dir ".scope-lock.edn" boundary))))

      (testing "does NOT find config above boundary"
        (spit (str tmp-dir "/.scope-lock.edn") "{:allowed-paths [\"evil/\"]}")
        ;; Walking from sub-dir with boundary at project/ should find project config,
        ;; not the one above it
        (is (= (str tmp-dir "/project/.scope-lock.edn")
               (config/find-config-up sub-dir ".scope-lock.edn" boundary))))

      (testing "returns nil when config only exists above boundary"
        (fs/delete (str tmp-dir "/project/.scope-lock.edn"))
        ;; Now the only config is above the boundary — should not find it
        (is (nil? (config/find-config-up sub-dir ".scope-lock.edn" boundary))))
      (finally
        (fs/delete-tree tmp-dir)))))

(deftest test-load-edn
  (let [tmp (str (fs/create-temp-file {:prefix "edn-test-" :suffix ".edn"}))]
    (try
      (spit tmp "{:foo :bar :n 42}")
      (testing "loads valid EDN"
        (is (= {:foo :bar :n 42} (config/load-edn tmp))))

      (testing "returns nil for nonexistent file"
        (is (nil? (config/load-edn "/nonexistent/file.edn"))))

      (testing "returns nil for nil path"
        (is (nil? (config/load-edn nil))))
      (finally
        (fs/delete tmp)))))

(deftest test-deep-merge
  (testing "merges nested maps"
    (is (= {:a {:b 2 :c 3} :d 4}
           (config/deep-merge {:a {:b 1 :c 3}} {:a {:b 2} :d 4}))))

  (testing "later values win for non-map keys"
    (is (= {:a 2} (config/deep-merge {:a 1} {:a 2}))))

  (testing "handles nil maps"
    (is (= {:a 1} (config/deep-merge nil {:a 1} nil)))))
