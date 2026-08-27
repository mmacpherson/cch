(ns cli.codex-app-server-service-test
  (:require [babashka.fs :as fs]
            [cli.codex-app-server-service :as service]
            [clojure.string :as str]
            [clojure.test :refer [deftest is]]))

(deftest render-unit-runs-only-the-local-unix-socket
  (let [unit (service/render-unit "/usr/bin/codex"
                                  "/home/example/.config/codex"
                                  "/home/example/bin:/usr/bin")]
    (is (str/includes? unit "ExecStart=\"/usr/bin/codex\" app-server --listen unix://"))
    (is (str/includes? unit "Environment=\"CODEX_HOME=/home/example/.config/codex\""))
    (is (str/includes? unit "Environment=\"PATH=/home/example/bin:/usr/bin\""))
    (is (str/includes? unit "Restart=on-failure"))
    (is (not (str/includes? unit "--remote-control"))
        "the POC must not enroll in Codex Remote Control")
    (is (not (str/includes? unit "standalone"))
        "the service must follow the package manager's binary")))

(deftest render-unit-escapes-systemd-values
  (let [unit (service/render-unit "/opt/100%/co\"dex"
                                  "/home/u/code%x"
                                  "/home/u/100%/bin")]
    (is (str/includes? unit "/opt/100%%/co\\\"dex"))
    (is (str/includes? unit "/home/u/code%%x"))
    (is (str/includes? unit "/home/u/100%%/bin"))))

(deftest install-writes-enables-and-starts-an-inactive-user-service
  (let [home (str (fs/create-temp-dir {:prefix "cch-codex-service-"}))
        calls (atom [])
        resolve-command (fn [command _path]
                          ({"codex" "/usr/bin/codex"
                            "systemctl" "/usr/bin/systemctl"} command))]
    (try
      (let [result (service/install!
                     {:os-name "Linux"
                      :home home
                      :codex-home "/home/example/.config/codex"
                      :path "/usr/bin"
                      :resolve-command resolve-command
                      :run-command (fn [argv]
                                     (swap! calls conj argv)
                                     {:exit (cond
                                              (= "is-active" (nth argv 2)) 3
                                              (= "is-enabled" (nth argv 2)) 1
                                              :else 0)
                                      :out "" :err ""})
                      :wait-ready (fn [_] true)})
            unit-path (service/service-path home)]
        (is (= :installed (:status result)))
        (is (fs/exists? unit-path))
        (is (= [["/usr/bin/systemctl" "--user" "is-active" "--quiet"
                 service/service-name]
                ["/usr/bin/systemctl" "--user" "is-enabled" "--quiet"
                 service/service-name]
                ["/usr/bin/systemctl" "--user" "daemon-reload"]
                ["/usr/bin/systemctl" "--user" "enable" service/service-name]
                ["/usr/bin/systemctl" "--user" "start" service/service-name]]
               @calls)))
      (finally
        (fs/delete-tree home)))))

(deftest reinstall-leaves-an-active-unchanged-service-untouched
  (let [home (str (fs/create-temp-dir {:prefix "cch-codex-service-"}))
        unit-path (service/service-path home)
        unit (service/render-unit "/usr/bin/codex"
                                  "/home/example/.config/codex"
                                  "/usr/bin")
        calls (atom [])
        ready (atom [])]
    (try
      (fs/create-dirs (fs/parent unit-path))
      (spit unit-path unit)
      (let [result (service/install!
                     {:os-name "Linux"
                      :home home
                      :codex-home "/home/example/.config/codex"
                      :path "/usr/bin"
                      :resolve-command (fn [command _] (str "/usr/bin/" command))
                      :run-command (fn [argv]
                                     (swap! calls conj argv)
                                     {:exit 0 :out "" :err ""})
                      :wait-ready (fn [path] (swap! ready conj path) true)})]
        (is (= :unchanged (:status result)))
        (is (= unit (slurp unit-path)))
        (is (= [["/usr/bin/systemctl" "--user" "is-active" "--quiet"
                 service/service-name]
                ["/usr/bin/systemctl" "--user" "is-enabled" "--quiet"
                 service/service-name]]
               @calls))
        (is (= ["/home/example/.config/codex/app-server-control/app-server-control.sock"]
               @ready)))
      (finally
        (fs/delete-tree home)))))

(deftest changed-unit-does-not-restart-an-active-service
  (let [home (str (fs/create-temp-dir {:prefix "cch-codex-service-"}))
        unit-path (service/service-path home)
        calls (atom [])]
    (try
      (fs/create-dirs (fs/parent unit-path))
      (spit unit-path "old synthetic unit\n")
      (let [result (service/install!
                     {:os-name "Linux"
                      :home home
                      :codex-home "/home/example/.config/codex"
                      :path "/usr/bin"
                      :resolve-command (fn [command _] (str "/usr/bin/" command))
                      :run-command (fn [argv]
                                     (swap! calls conj argv)
                                     {:exit 0 :out "" :err ""})
                      :wait-ready (fn [_] true)})]
        (is (= :updated-restart-required (:status result)))
        (is (str/includes? (slurp unit-path) "app-server --listen unix://"))
        (is (= [["/usr/bin/systemctl" "--user" "is-active" "--quiet"
                 service/service-name]
                ["/usr/bin/systemctl" "--user" "is-enabled" "--quiet"
                 service/service-name]
                ["/usr/bin/systemctl" "--user" "daemon-reload"]]
               @calls))
        (is (not-any? #(contains? #{"start" "restart"} (nth % 2)) @calls)))
      (finally
        (fs/delete-tree home)))))

(deftest reinstall-starts-an-inactive-unchanged-service
  (let [home (str (fs/create-temp-dir {:prefix "cch-codex-service-"}))
        unit-path (service/service-path home)
        unit (service/render-unit "/usr/bin/codex"
                                  "/home/example/.config/codex"
                                  "/usr/bin")
        calls (atom [])]
    (try
      (fs/create-dirs (fs/parent unit-path))
      (spit unit-path unit)
      (let [result (service/install!
                     {:os-name "Linux"
                      :home home
                      :codex-home "/home/example/.config/codex"
                      :path "/usr/bin"
                      :resolve-command (fn [command _] (str "/usr/bin/" command))
                      :run-command (fn [argv]
                                     (swap! calls conj argv)
                                     {:exit (if (= "is-active" (nth argv 2)) 3 0)
                                      :out "" :err ""})
                      :wait-ready (fn [_] true)})]
        (is (= :started (:status result)))
        (is (= [["/usr/bin/systemctl" "--user" "is-active" "--quiet"
                 service/service-name]
                ["/usr/bin/systemctl" "--user" "is-enabled" "--quiet"
                 service/service-name]
                ["/usr/bin/systemctl" "--user" "start" service/service-name]]
               @calls)))
      (finally
        (fs/delete-tree home)))))

(deftest install-does-nothing-outside-linux
  (let [called? (atom false)
        result (service/install!
                 {:os-name "Mac OS X"
                  :home "/home/example"
                  :codex-home "/home/example/.codex"
                  :path "/usr/bin"
                  :resolve-command (fn [& _] (reset! called? true))
                  :run-command (fn [& _] (reset! called? true))
                  :wait-ready (fn [& _] (reset! called? true))})]
    (is (= {:status :unsupported :reason :requires-linux-systemd} result))
    (is (false? @called?))))

(deftest failed-systemctl-command-is-actionable
  (let [home (str (fs/create-temp-dir {:prefix "cch-codex-service-"}))]
    (try
      (let [error (try
                    (service/install!
                      {:os-name "Linux"
                       :home home
                       :codex-home "/home/example/.codex"
                       :path "/usr/bin"
                       :resolve-command (fn [command _]
                                          (str "/usr/bin/" command))
                       :run-command (fn [_]
                                      {:exit 1 :out "" :err "no user bus"})
                       :wait-ready (fn [_] true)})
                    nil
                    (catch clojure.lang.ExceptionInfo e e))]
        (is (= :codex-service-command-failed (:type (ex-data error))))
        (is (str/includes? (.getMessage error) "no user bus")))
      (finally
        (fs/delete-tree home)))))
