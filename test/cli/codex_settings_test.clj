(ns cli.codex-settings-test
  (:require [clojure.test :refer [deftest is testing]]
            [cli.codex-settings :as cs]
            [babashka.fs :as fs]
            [clojure.string :as str]))

(defn- with-tmp [f]
  (let [tmp (str (fs/create-temp-file {:prefix "codex-" :suffix ".toml"}))]
    (try (f tmp) (finally (fs/delete tmp)))))

(def sample-entries
  [{:event "PreToolUse" :matcher ".*"
    :command "curl -s -X POST --data-binary @- http://127.0.0.1:8888/dispatch/PreToolUse"
    :timeout 30}])

(deftest render-block-has-sentinels
  (let [text (cs/render-block "doubled-token" sample-entries)]
    (is (str/starts-with? text "# cch:begin doubled-token\n"))
    (is (str/ends-with? text "# cch:end doubled-token\n"))
    (is (str/includes? text "[[hooks.PreToolUse]]"))
    (is (str/includes? text "matcher = \".*\""))
    (is (str/includes? text "type = \"command\""))
    (is (str/includes? text "timeout = 30"))))

(deftest render-block-escapes-quotes
  (let [entries [{:event "PreToolUse" :matcher "Bash"
                  :command "echo \"hi\""}]
        text (cs/render-block "x" entries)]
    (is (str/includes? text "command = \"echo \\\"hi\\\"\""))))

(deftest install-into-empty-file
  (with-tmp
    (fn [tmp]
      (spit tmp "")
      (cs/install-hook! tmp "doubled-token" sample-entries)
      (let [contents (slurp tmp)]
        (is (str/includes? contents "# cch:begin doubled-token"))
        (is (str/includes? contents "[[hooks.PreToolUse]]"))))))

(deftest install-preserves-user-content-byte-for-byte
  (with-tmp
    (fn [tmp]
      (let [user-content (str "# user comment\n"
                              "model = \"gpt-5.5\"\n"
                              "\n"
                              "[projects.\"/tmp/foo\"]\n"
                              "trust_level = \"trusted\"\n")]
        (spit tmp user-content)
        (cs/install-hook! tmp "doubled-token" sample-entries)
        (let [final (slurp tmp)]
          (is (str/starts-with? final user-content)
              "user lines preserved unchanged at start of file")
          (is (str/includes? final "# cch:begin doubled-token")))))))

(deftest install-is-idempotent-replaces-prior-block
  (with-tmp
    (fn [tmp]
      (spit tmp "model = \"gpt-5.5\"\n")
      (cs/install-hook! tmp "doubled-token" sample-entries)
      ;; Reinstall with a different command — should replace, not duplicate.
      (let [new-entries [{:event "PreToolUse" :matcher ".*"
                          :command "echo changed"}]]
        (cs/install-hook! tmp "doubled-token" new-entries)
        (let [final (slurp tmp)
              begin-count (count (re-seq #"# cch:begin doubled-token" final))]
          (is (= 1 begin-count) "only one block remains after reinstall")
          (is (str/includes? final "echo changed"))
          (is (not (str/includes? final "curl"))))))))

(deftest install-leaves-other-cch-blocks-alone
  (with-tmp
    (fn [tmp]
      (spit tmp "")
      (cs/install-hook! tmp "hook-a" sample-entries)
      (cs/install-hook! tmp "hook-b" sample-entries)
      ;; Reinstall a only.
      (cs/install-hook! tmp "hook-a" [{:event "PostToolUse" :matcher "Bash"
                                       :command "echo a"}])
      (let [final (slurp tmp)]
        (is (str/includes? final "# cch:begin hook-a"))
        (is (str/includes? final "# cch:begin hook-b"))
        (is (str/includes? final "echo a"))
        (is (str/includes? final "[[hooks.PostToolUse]]"))))))

(deftest remove-hook-strips-only-named-block
  (with-tmp
    (fn [tmp]
      (spit tmp "model = \"gpt-5.5\"\n")
      (cs/install-hook! tmp "hook-a" sample-entries)
      (cs/install-hook! tmp "hook-b" sample-entries)
      (cs/remove-hook! tmp "hook-a")
      (let [final (slurp tmp)]
        (is (not (str/includes? final "# cch:begin hook-a")))
        (is (str/includes? final "# cch:begin hook-b"))
        (is (str/starts-with? final "model = \"gpt-5.5\"\n"))))))

(deftest remove-all-strips-every-cch-block
  (with-tmp
    (fn [tmp]
      (spit tmp "model = \"gpt-5.5\"\n")
      (cs/install-hook! tmp "hook-a" sample-entries)
      (cs/install-hook! tmp "hook-b" sample-entries)
      (cs/remove-all-cch! tmp)
      (let [final (slurp tmp)]
        (is (not (str/includes? final "cch:")))
        (is (str/starts-with? final "model"))))))

(deftest preserves-user-hooks-that-are-not-cch-tagged
  (with-tmp
    (fn [tmp]
      (let [user-hook (str "[[hooks.PreToolUse]]\n"
                           "matcher = \"Bash\"\n"
                           "[[hooks.PreToolUse.hooks]]\n"
                           "type = \"command\"\n"
                           "command = \"echo user-wrote-this\"\n")]
        (spit tmp user-hook)
        (cs/install-hook! tmp "doubled-token" sample-entries)
        (cs/remove-hook! tmp "doubled-token")
        (is (= user-hook (slurp tmp))
            "user hook block survives install+uninstall round-trip")))))

(deftest read-nonexistent-returns-empty-string
  (is (= "" (cs/read-config "/nonexistent/path/config.toml"))))

(deftest write-creates-parent-dirs
  (let [dir (str (fs/create-temp-dir {:prefix "codex-test-"}))
        path (str dir "/nested/config.toml")]
    (try
      (cs/write-config! path "model = \"gpt-5.5\"\n")
      (is (= "model = \"gpt-5.5\"\n" (slurp path)))
      (finally
        (fs/delete-tree dir)))))
