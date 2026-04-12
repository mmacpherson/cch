(ns cch.config-test
  (:require [clojure.test :refer [deftest is testing]]
            [cch.config :as config]
            [babashka.fs :as fs]))

(deftest test-find-config-up
  (let [tmp-dir (str (fs/create-temp-dir {:prefix "config-test-"}))
        sub-dir (str tmp-dir "/a/b/c")]
    (try
      (fs/create-dirs sub-dir)
      (spit (str tmp-dir "/.cch-config.yaml") "log:\n  enabled: true\n")

      (testing "finds config walking up from subdirectory"
        (is (= (str tmp-dir "/.cch-config.yaml")
               (config/find-config-up sub-dir ".cch-config.yaml"))))

      (testing "returns nil when config not found"
        (let [isolated (str (fs/create-temp-dir {:prefix "no-config-"}))]
          (try
            (is (nil? (config/find-config-up isolated ".nonexistent.yaml")))
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
        (spit (str tmp-dir "/project/.cch-config.yaml")
              "hooks:\n  scope-lock:\n    allowed-paths:\n      - src/\n")
        (is (= (str tmp-dir "/project/.cch-config.yaml")
               (config/find-config-up sub-dir ".cch-config.yaml" boundary))))

      (testing "does NOT find config above boundary"
        (spit (str tmp-dir "/.cch-config.yaml")
              "hooks:\n  scope-lock:\n    allowed-paths:\n      - evil/\n")
        ;; Walking from sub-dir with boundary at project/ should find project config,
        ;; not the one above it
        (is (= (str tmp-dir "/project/.cch-config.yaml")
               (config/find-config-up sub-dir ".cch-config.yaml" boundary))))

      (testing "returns nil when config only exists above boundary"
        (fs/delete (str tmp-dir "/project/.cch-config.yaml"))
        ;; Now the only config is above the boundary — should not find it
        (is (nil? (config/find-config-up sub-dir ".cch-config.yaml" boundary))))
      (finally
        (fs/delete-tree tmp-dir)))))

(deftest test-load-yaml
  (let [tmp (str (fs/create-temp-file {:prefix "yaml-test-" :suffix ".yaml"}))]
    (try
      (spit tmp "foo: bar\nn: 42\nitems:\n  - a\n  - b\n")
      (testing "loads valid YAML with keyword keys"
        (is (= {:foo "bar" :n 42 :items ["a" "b"]} (config/load-yaml tmp))))

      (testing "returns nil for nonexistent file"
        (is (nil? (config/load-yaml "/nonexistent/file.yaml"))))

      (testing "returns nil for nil path"
        (is (nil? (config/load-yaml nil))))

      (testing "throws ex-info with ::malformed-config on bad YAML"
        (spit tmp "key: [unclosed\n")
        (let [ex (try
                   (config/load-yaml tmp)
                   nil
                   (catch clojure.lang.ExceptionInfo e e))]
          (is (some? ex))
          (is (= :cch.config/malformed-config (:type (ex-data ex))))
          (is (= tmp (:path (ex-data ex))))))
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
