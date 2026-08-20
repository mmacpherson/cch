(ns cli.install-test
  "Unit tests for cli.install flag handling — in particular that unknown
  flags (like --help) no longer fall through to a real mutating install.
  See claude-code-hooks-5sg."
  (:require [clojure.test :refer [deftest is testing]]
            [cli.install :as install]))

(def ^:private help?        #'install/help?)
(def ^:private unknown-flags #'install/unknown-flags)
(def ^:private parse-flags   #'install/parse-flags)

(defn- flags-of [args]
  (first (parse-flags args)))

(deftest help-detection
  (testing "--help and -h are recognized"
    (is (true? (help? ["--help"])))
    (is (true? (help? ["-h"])))
    (is (true? (help? ["--codex" "--help"]))))
  (testing "no help flag present"
    (is (false? (help? [])))
    (is (false? (help? ["--codex"])))
    (is (false? (help? ["--global"])))))

(deftest unknown-flag-detection
  (testing "known flags are never reported as unknown"
    (is (nil? (unknown-flags (flags-of []))))
    (is (nil? (unknown-flags (flags-of ["--codex"]))))
    (is (nil? (unknown-flags (flags-of ["--agy"]))))
    (is (nil? (unknown-flags (flags-of ["--all"]))))
    (is (nil? (unknown-flags (flags-of ["--global"])))))
  (testing "--help is allowed through, not treated as unknown"
    (is (nil? (unknown-flags (flags-of ["--help"])))))
  (testing "genuinely unknown flags are surfaced"
    (is (= ["--bogus"] (vec (unknown-flags (flags-of ["--bogus"])))))
    (is (= ["--nope"]  (vec (unknown-flags (flags-of ["--codex" "--nope"])))))))

(deftest all-flag-dispatch
  ;; The --all branch returns normally (no System/exit), so we can drive `run`
  ;; with the real installer stubbed out and assert the Claude-target choice.
  (let [called (atom nil)]
    (with-redefs [cli.install/run-all-install! (fn [global?] (reset! called global?))]
      (testing "--all provisions with the repo-local Claude target by default"
        (reset! called :unset)
        (install/run "--all")
        (is (false? @called)))
      (testing "--all --global selects the global Claude target"
        (reset! called :unset)
        (install/run "--all" "--global")
        (is (true? @called))))))
