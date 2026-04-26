(ns cli.service-cmd-test
  "Tests for cch install-service / uninstall-service.

  Coverage focuses on the pure pieces — template rendering, platform
  descriptor shapes, OS classification. No tests actually load a systemd
  unit or a launchd plist; that needs a real OS subsystem and isn't
  portable in CI."
  (:require [cli.service-cmd :as svc]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]))

;; --- Template rendering ---

(deftest linux-template-is-unmodified-by-home-substitution
  (testing "systemd template uses %h internally; {{HOME}} substitution leaves the file unchanged"
    (let [raw      (slurp (io/resource "service/cch.service.template"))
          rendered (#'svc/render-template "service/cch.service.template")]
      ;; Template has no {{HOME}} tokens — render-template's replace is a no-op.
      (is (= raw rendered))
      (is (str/includes? rendered "WorkingDirectory=%h/.local/share/cch/repo")
          "systemd %h must be in the rendered unit")
      (is (str/includes? rendered "Restart=on-failure")
          "Restart policy must be present"))))

(deftest macos-template-has-home-baked-in
  (testing "launchd plist — {{HOME}} gets substituted with user.home at render time"
    (let [rendered (#'svc/render-template "service/com.cch.server.plist.template")
          home     (System/getProperty "user.home")]
      (is (not (str/includes? rendered "{{HOME}}"))
          "no unresolved {{HOME}} tokens remain")
      (is (str/includes? rendered (str home "/.local/share/cch/repo"))
          "home directory is baked into the working directory")
      (is (str/includes? rendered (str home "/.local/share/cch/serve.log"))
          "home directory is baked into the log path")
      (is (str/includes? rendered "<key>RunAtLoad</key>")
          "plist preserves the KeepAlive/RunAtLoad keys"))))

;; --- OS classification ---

(deftest host-os-returns-a-known-keyword
  (testing "host-os returns one of :linux, :macos, :unsupported"
    (is (contains? #{:linux :macos :unsupported} (#'svc/host-os)))))

;; --- Platform descriptors ---

(deftest linux-descriptor-shape
  (testing "all linux keys are present and point at expected locations"
    (let [info (#'svc/platform-info :linux)]
      (is (= "systemd user unit" (:label info)))
      (is (str/ends-with? (:path info) "/.config/systemd/user/cch.service"))
      (is (= "service/cch.service.template" (:template info)))
      (is (= "systemctl --user enable --now cch" (:activate info)))
      (is (= "systemctl --user disable --now cch" (:disable info)))
      (is (str/includes? (:logs info) "journalctl")))))

(deftest macos-descriptor-shape
  (testing "all macos keys are present and use modern launchctl bootstrap"
    (let [info (#'svc/platform-info :macos)]
      (is (= "LaunchAgent" (:label info)))
      (is (str/ends-with? (:path info) "/Library/LaunchAgents/com.cch.server.plist"))
      (is (= "service/com.cch.server.plist.template" (:template info)))
      (is (str/includes? (:activate info) "launchctl bootstrap gui/")
          "uses the modern bootstrap syntax, not the deprecated `launchctl load`")
      (is (str/includes? (:disable info) "launchctl bootout gui/"))
      (is (str/includes? (:logs info) "serve.log")))))
