(ns cli.control-runner-service-test
  (:require [babashka.fs :as fs]
            [cli.control-runner-service :as service]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]))

(deftest service-templates-use-absolute-runtime-without-pairing-secrets
  (let [systemd (service/render-systemd
                  "/opt/cch/bin/cch" "/opt/cch/bin:/usr/bin"
                  "/home/example/.config/cch/control-runner.json")
        launchd (service/render-launchd "/opt/cch/bin/cch"
                                        "/Users/example"
                                        "/opt/cch/bin:/usr/bin"
                                        "/Users/example/.config/cch/control-runner.json")]
    (is (str/includes? systemd
                       "ExecStart=\"/opt/cch/bin/cch\" control runner"))
    (is (str/includes? launchd "<string>/opt/cch/bin/cch</string>"))
    (is (str/includes? systemd
                       "CCH_CONTROL_PAIRING_PATH=/home/example/.config/cch/control-runner.json"))
    (is (str/includes? launchd
                       "<string>/Users/example/.config/cch/control-runner.json</string>"))
    (is (not (re-find #"TOKEN|BROKER_URL|RUNNER_ID" systemd)))
    (is (not (re-find #"TOKEN|BROKER_URL|RUNNER_ID" launchd)))))

(deftest linux-install-enables-and-starts-an-inactive-runner
  (let [home (str (fs/create-temp-dir {:prefix "cch-runner-linux-test-"}))
        calls (atom [])
        resolve-command (fn [command _]
                          (get {"cch" "/opt/cch/bin/cch"
                                "systemctl" "/usr/bin/systemctl"} command))]
    (try
      (let [result
            (service/install!
              {:os-name "Linux" :home home :path "/usr/bin"
               :resolve-command resolve-command
               :run-command
               (fn [argv]
                 (swap! calls conj argv)
                 {:exit (if (some #{"is-active" "is-enabled"} argv) 1 0)
                  :out "" :err ""})})]
        (is (= :installed (:status result)))
        (is (= "/opt/cch/bin/cch" (:cch-bin result)))
        (is (str/includes? (slurp (service/service-path :linux home))
                           "ExecStart=\"/opt/cch/bin/cch\" control runner"))
        (is (= [["/usr/bin/systemctl" "--user" "is-active" "--quiet"
                 service/linux-service-name]
                ["/usr/bin/systemctl" "--user" "is-enabled" "--quiet"
                 service/linux-service-name]
                ["/usr/bin/systemctl" "--user" "daemon-reload"]
                ["/usr/bin/systemctl" "--user" "enable"
                 service/linux-service-name]
                ["/usr/bin/systemctl" "--user" "start"
                 service/linux-service-name]]
               @calls)))
      (finally
        (fs/delete-tree home)))))

(deftest macos-install-bootstraps-launchagent-with-complete-path
  (let [home (str (fs/create-temp-dir {:prefix "cch-runner-macos-test-"}))
        calls (atom [])
        result
        (service/install!
          {:os-name "Mac OS X" :home home :path "/opt/homebrew/bin:/usr/bin"
           :uid "501"
           :resolve-command (fn [command _]
                              (get {"cch" "/opt/homebrew/bin/cch"
                                    "launchctl" "/bin/launchctl"} command))
           :run-command
           (fn [argv]
             (swap! calls conj argv)
             {:exit (if (= "print" (second argv)) 1 0) :out "" :err ""})})]
    (try
      (is (= :installed (:status result)))
      (let [plist (slurp (service/service-path :macos home))]
        (is (str/includes? plist "<string>/opt/homebrew/bin/cch</string>"))
        (is (str/includes? plist
                           "<string>/opt/homebrew/bin:/usr/bin</string>")))
      (is (= [["/bin/launchctl" "print"
               (str "gui/501/" service/macos-label)]
              ["/bin/launchctl" "bootstrap" "gui/501"
               (service/service-path :macos home)]]
             @calls))
      (finally
        (fs/delete-tree home)))))

(deftest changed-active-runner-is-never-restarted-implicitly
  (doseq [[os resolve-command run-command path]
          [["Linux"
            (fn [command _]
              (get {"cch" "/opt/cch/bin/cch"
                    "systemctl" "/usr/bin/systemctl"} command))
            (fn [_] {:exit 0 :out "" :err ""})
            "/usr/bin"]
           ["Mac OS X"
            (fn [command _]
              (get {"cch" "/opt/cch/bin/cch"
                    "launchctl" "/bin/launchctl"} command))
            (fn [_] {:exit 0 :out "" :err ""})
            "/opt/homebrew/bin:/usr/bin"]]]
    (testing os
      (let [home (str (fs/create-temp-dir {:prefix "cch-runner-active-test-"}))
            calls (atom [])]
        (try
          (let [result
                (service/install!
                  {:os-name os :home home :path path :uid "501"
                   :resolve-command resolve-command
                   :run-command (fn [argv]
                                  (swap! calls conj argv)
                                  (run-command argv))})]
            (is (= :updated-restart-required (:status result)))
            (is (not-any? #(some #{"restart" "start" "bootstrap"} %)
                          @calls)))
          (finally
            (fs/delete-tree home)))))))

(deftest supervision-status-is-sanitized
  (is (= {:status :active :service service/linux-service-name}
         (service/status
           {:os-name "Linux" :path "/usr/bin"
            :resolve-command (fn [command _]
                               (when (= "systemctl" command)
                                 "/usr/bin/systemctl"))
            :run-command (constantly {:exit 0 :out "" :err ""})
            :uid nil}))))
