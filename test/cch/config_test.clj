(ns cch.config-test
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [cch.config :as config]
            [cch.config-db :as cdb]
            [cch.db :as db]
            [cch.log :as log]
            [babashka.fs :as fs]
            [babashka.process :as p]
            [clojure.string :as str]))

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

;; --- load-effective-config (dispatcher precedence) ---
;;
;; Tests use a tmp DB (via with-redefs on db/db-path) and a tmp git
;; repo root so worktree-root resolves somewhere controlled.

(def ^:dynamic *tmp-db*     nil)
(def ^:dynamic *tmp-repo*   nil)

(defn- init-tmp-repo! []
  (let [tmp (str (fs/create-temp-dir {:prefix "cfg-eff-"}))]
    (p/sh {:dir tmp} "git" "init" "-q")
    ;; Canonicalize so worktree-root (which calls git rev-parse) returns
    ;; the same path the test uses (on macOS /tmp is a symlink).
    (str/trim (:out (p/sh {:dir tmp} "git" "rev-parse" "--show-toplevel")))))

(defn with-effective-env [t]
  (let [tmp-dir (str (fs/create-temp-dir {:prefix "cfg-eff-db-"}))
        db      (str tmp-dir "/events.db")
        repo    (init-tmp-repo!)]
    (try
      (binding [*tmp-db* db
                *tmp-repo* repo]
        (with-redefs [db/db-path (fn [] db)]
          (t)))
      (finally
        (fs/delete-tree tmp-dir)
        (fs/delete-tree repo)))))

(use-fixtures :each with-effective-env)

(deftest test-effective-defaults-only
  (testing "no YAML, no DB rows → every registered hook is enabled via :default"
    (let [{:keys [hooks yaml-path yaml-error]} (config/load-effective-config *tmp-repo*)]
      (is (nil? yaml-path))
      (is (nil? yaml-error))
      ;; Pick a hook that we know is registered:
      (is (contains? hooks "scope-lock"))
      (is (true? (get-in hooks ["scope-lock" :enabled?])))
      (is (= :default (get-in hooks ["scope-lock" :source]))))))

(deftest test-effective-db-global-overrides-defaults
  (testing "DB global row overrides the default for that hook"
    (cdb/upsert! {:hook-name "scope-lock"
                  :scope     cdb/global-scope
                  :enabled   false
                  :options   {:allowed-paths ["src/"]}})
    (let [{:keys [hooks]} (config/load-effective-config *tmp-repo*)]
      (is (false? (get-in hooks ["scope-lock" :enabled?])))
      (is (= :db-global (get-in hooks ["scope-lock" :source])))
      (is (= {:allowed-paths ["src/"]} (get-in hooks ["scope-lock" :options]))))))

(deftest test-effective-db-repo-overrides-global
  (testing "DB repo row overrides DB global"
    (cdb/upsert! {:hook-name "scope-lock" :scope cdb/global-scope            :enabled true})
    (cdb/upsert! {:hook-name "scope-lock" :scope (cdb/repo-scope *tmp-repo*) :enabled false})
    (let [{:keys [hooks]} (config/load-effective-config *tmp-repo*)]
      (is (false? (get-in hooks ["scope-lock" :enabled?])))
      (is (= :db-repo (get-in hooks ["scope-lock" :source]))))))

(deftest test-effective-yaml-beats-db-repo
  (testing "repo YAML present → DB repo rows ignored for that repo"
    (cdb/upsert! {:hook-name "scope-lock" :scope (cdb/repo-scope *tmp-repo*) :enabled false})
    (spit (str *tmp-repo* "/.cch-config.yaml")
          "hooks:\n  scope-lock:\n    allowed-paths:\n      - src/\n")
    (let [{:keys [hooks yaml-path]} (config/load-effective-config *tmp-repo*)]
      (is (some? yaml-path))
      ;; YAML doesn't set :enabled false — so it's enabled (per presence-semantics)
      (is (true? (get-in hooks ["scope-lock" :enabled?])))
      (is (= :repo-yaml (get-in hooks ["scope-lock" :source])))
      (is (= {:allowed-paths ["src/"]} (get-in hooks ["scope-lock" :options]))))))

(deftest test-effective-yaml-can-disable
  (testing ":enabled false in YAML disables the hook"
    (spit (str *tmp-repo* "/.cch-config.yaml")
          "hooks:\n  scope-lock:\n    enabled: false\n")
    (let [{:keys [hooks]} (config/load-effective-config *tmp-repo*)]
      (is (false? (get-in hooks ["scope-lock" :enabled?]))))))

(deftest test-effective-yaml-preserves-db-global-for-unmentioned-hooks
  (testing "hook NOT in YAML falls through to DB global / defaults"
    (cdb/upsert! {:hook-name "protect-files" :scope cdb/global-scope :enabled false})
    (spit (str *tmp-repo* "/.cch-config.yaml")
          "hooks:\n  scope-lock:\n    allowed-paths:\n      - src/\n")
    (let [{:keys [hooks]} (config/load-effective-config *tmp-repo*)]
      (is (= :repo-yaml (get-in hooks ["scope-lock"    :source])))
      (is (= :db-global (get-in hooks ["protect-files" :source])))
      (is (false? (get-in hooks ["protect-files" :enabled?]))))))

(deftest test-effective-malformed-yaml-sets-error-and-skips-layer
  (testing "malformed YAML → :yaml-error set, YAML layer skipped, DB still applies"
    (cdb/upsert! {:hook-name "scope-lock" :scope cdb/global-scope :enabled false})
    (spit (str *tmp-repo* "/.cch-config.yaml") "hooks: [unclosed\n")
    (let [{:keys [hooks yaml-error]} (config/load-effective-config *tmp-repo*)]
      (is (some? yaml-error))
      (is (str/includes? yaml-error "malformed"))
      ;; DB global still applies:
      (is (= :db-global (get-in hooks ["scope-lock" :source])))
      (is (false? (get-in hooks ["scope-lock" :enabled?]))))))
