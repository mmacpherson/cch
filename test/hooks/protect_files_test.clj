(ns hooks.protect-files-test
  (:require [babashka.fs :as fs]
            [babashka.process :as p]
            [cheshire.core :as json]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [hooks.protect-files :as pf]))

(def repo-root
  (str/trim (:out (p/sh ["git" "rev-parse" "--show-toplevel"]))))

;; --- Unit tests: check-path against default patterns ---

(deftest test-denies-dot-env
  (testing ".env at root"
    (let [result (pf/check-path "/repo/.env" pf/default-patterns)]
      (is (= :deny (:decision result)))
      (is (str/includes? (:reason result) "sensitive")))))

(deftest test-denies-dot-env-local
  (testing ".env.local variant"
    (is (= :deny (:decision (pf/check-path "/repo/.env.local" pf/default-patterns))))))

(deftest test-denies-nested-env
  (testing ".env deep in a subtree"
    (is (= :deny (:decision (pf/check-path "/a/b/c/.env" pf/default-patterns))))))

(deftest test-denies-dot-git-contents
  (is (= :deny (:decision (pf/check-path "/repo/.git/config" pf/default-patterns)))))

(deftest test-denies-ssh-keys
  (testing "id_rsa and id_ed25519"
    (is (= :deny (:decision (pf/check-path "/home/me/.ssh/id_rsa" pf/default-patterns))))
    (is (= :deny (:decision (pf/check-path "/home/me/.ssh/id_ed25519" pf/default-patterns))))))

(deftest test-denies-ssh-anything
  (testing "anything under .ssh/"
    (is (= :deny (:decision (pf/check-path "/home/me/.ssh/known_hosts" pf/default-patterns))))))

(deftest test-denies-pem-and-key
  (is (= :deny (:decision (pf/check-path "/etc/ssl/server.pem" pf/default-patterns))))
  (is (= :deny (:decision (pf/check-path "/etc/ssl/server.key" pf/default-patterns)))))

(deftest test-denies-secrets-dir
  (is (= :deny (:decision (pf/check-path "/repo/secrets/db.txt" pf/default-patterns)))))

(deftest test-denies-aws-credentials
  (is (= :deny (:decision (pf/check-path "/home/me/.aws/credentials" pf/default-patterns)))))

(deftest test-denies-netrc-npmrc
  (is (= :deny (:decision (pf/check-path "/home/me/.netrc" pf/default-patterns))))
  (is (= :deny (:decision (pf/check-path "/home/me/.npmrc" pf/default-patterns)))))

(deftest test-denies-keystores
  (is (= :deny (:decision (pf/check-path "/repo/vault.kdbx" pf/default-patterns))))
  (is (= :deny (:decision (pf/check-path "/repo/app.keystore" pf/default-patterns)))))

;; --- Legitimate paths pass ---

(deftest test-allows-source-files
  (testing "normal source paths return nil"
    (is (nil? (pf/check-path "/repo/src/main.py" pf/default-patterns)))
    (is (nil? (pf/check-path "/repo/README.md" pf/default-patterns)))
    (is (nil? (pf/check-path "/repo/package.json" pf/default-patterns)))))

(deftest test-env-substring-in-filename-passes
  (testing "files that merely contain 'env' but aren't .env are allowed"
    (is (nil? (pf/check-path "/repo/envoy.yaml" pf/default-patterns)))
    (is (nil? (pf/check-path "/repo/environment.md" pf/default-patterns)))))

;; --- Nil / empty inputs ---

(deftest test-nil-path-is-nil
  (is (nil? (pf/check-path nil pf/default-patterns))))

(deftest test-empty-patterns-allows-all
  (is (nil? (pf/check-path "/repo/.env" []))))

;; --- extra-patterns and disable-defaults ---

(deftest test-extra-patterns-extend-deny-set
  (testing "custom glob added via extra-patterns blocks its matches"
    (let [cfg      {:extra-patterns ["**/vault/**"]}
          patterns (pf/effective-patterns cfg)]
      (is (= :deny (:decision (pf/check-path "/repo/vault/db.sql" patterns))))
      ;; Defaults still apply
      (is (= :deny (:decision (pf/check-path "/repo/.env" patterns)))))))

(deftest test-disable-defaults-drops-builtins
  (testing "disable-defaults true means .env no longer blocked if not in extras"
    (let [cfg      {:disable-defaults true
                    :extra-patterns   ["**/vault/**"]}
          patterns (pf/effective-patterns cfg)]
      (is (nil? (pf/check-path "/repo/.env" patterns)))
      (is (= :deny (:decision (pf/check-path "/repo/vault/db.sql" patterns)))))))

(deftest test-tilde-expands-in-extras
  (testing "~/.ssh/foo in extra-patterns matches actual $HOME path"
    (let [home     (System/getProperty "user.home")
          cfg      {:disable-defaults true
                    :extra-patterns   ["~/secrets-of-mine/**"]}
          patterns (pf/effective-patterns cfg)]
      (is (= :deny (:decision (pf/check-path (str home "/secrets-of-mine/foo.txt")
                                             patterns))))
      ;; Different user's home dir does NOT match — the expansion is literal to $HOME
      (is (nil? (pf/check-path "/home/other-user/secrets-of-mine/foo.txt" patterns))))))

(deftest test-invalid-glob-returns-deny
  (testing "unclosed bracket in glob produces a deny with diagnostic"
    (let [result (pf/check-path "/repo/foo.txt" ["[unclosed"])]
      (is (= :deny (:decision result)))
      (is (str/includes? (:reason result) "invalid glob")))))

;; --- Integration: run as subprocess like Claude Code does ---

(deftest test-cli-integration
  (let [run (fn [json-input]
              (p/sh {:dir repo-root
                     :in  json-input}
                    "bb" "-cp" "src:resources" "-m" "hooks.protect-files"))]

    (testing ".env edit returns JSON deny response"
      (let [input  (format "{\"cwd\":\"%s\",\"tool_input\":{\"file_path\":\"%s/.env\"}}"
                           repo-root repo-root)
            result (run input)
            parsed (json/parse-string (:out result) true)]
        (is (zero? (:exit result)))
        (is (= "deny" (get-in parsed [:hookSpecificOutput :permissionDecision])))
        (is (str/includes?
              (get-in parsed [:hookSpecificOutput :permissionDecisionReason])
              "sensitive"))))

    (testing "normal file edit exits 0 with no output"
      (let [input  (format "{\"cwd\":\"%s\",\"tool_input\":{\"file_path\":\"%s/src/cch/core.clj\"}}"
                           repo-root repo-root)
            result (run input)]
        (is (zero? (:exit result)))
        (is (str/blank? (:out result)))))

    (testing "malformed .cch-config.yaml fails closed"
      (let [tmp-repo (str (fs/create-temp-dir {:prefix "protect-malformed-"}))]
        (try
          (p/sh {:dir tmp-repo} "git" "init" "-q")
          (let [real-root (str/trim (:out (p/sh {:dir tmp-repo}
                                                "git" "rev-parse" "--show-toplevel")))]
            (spit (str real-root "/.cch-config.yaml")
                  "hooks:\n  protect-files:\n    extra-patterns: [unclosed\n")
            (fs/create-dirs (str real-root "/src"))
            (let [input  (format "{\"cwd\":\"%s\",\"tool_input\":{\"file_path\":\"%s/src/a.clj\"}}"
                                 real-root real-root)
                  result (p/sh {:dir real-root :in input}
                               "bb" "-cp" (str repo-root "/src:" repo-root "/resources")
                               "-m" "hooks.protect-files")
                  parsed (json/parse-string (:out result) true)]
              (is (zero? (:exit result)))
              (is (= "deny" (get-in parsed [:hookSpecificOutput :permissionDecision])))
              (is (str/includes?
                    (get-in parsed [:hookSpecificOutput :permissionDecisionReason])
                    "malformed config"))))
          (finally
            (fs/delete-tree tmp-repo)))))))
