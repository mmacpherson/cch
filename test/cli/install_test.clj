(ns cli.install-test
  "install/run now receives a parsed options map — the cli.cch dispatcher does
  the flag parsing (via tools.cli), so the old hand-rolled parse-flags/help?/
  unknown-flags tests moved there (see cli.cch-test). What remains cch-specific
  is install's target choice and mutual-exclusion, driven off the options map."
  (:require [clojure.test :refer [deftest is testing]]
            [cli.install :as install]))

(deftest all-flag-dispatch
  (let [called (atom :unset)]
    (with-redefs [cli.install/run-all-install! (fn [global?] (reset! called global?))]
      (testing "--all provisions with the repo-local Claude target by default"
        (reset! called :unset)
        (install/run {:all true} nil)
        (is (false? @called)))
      (testing "--all --global selects the global Claude target"
        (reset! called :unset)
        (install/run {:all true :global true} nil)
        (is (true? @called))))))

(deftest target-choice
  (testing "no flags → repo-local Claude install"
    (let [called (atom :unset)]
      (with-redefs [cli.install/run-claude-install! (fn [global?] (reset! called global?))]
        (install/run {} nil)
        (is (false? @called)))))
  (testing "--global → global Claude install"
    (let [called (atom :unset)]
      (with-redefs [cli.install/run-claude-install! (fn [global?] (reset! called global?))]
        (install/run {:global true} nil)
        (is (true? @called)))))
  (testing "--codex routes to the codex installer"
    (let [called (atom false)]
      (with-redefs [cli.install/run-codex-install! (fn [] (reset! called true))]
        (install/run {:codex true} nil)
        (is (true? @called))))))
