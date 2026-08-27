(ns cli.control-runner-service
  "Install the paired outbound control runner as a user service.

  The service reads its credential from cch's owner-only local pairing file;
  neither systemd nor launchd definitions contain the token. Reconciliation
  never restarts an already-active runner implicitly."
  (:require [babashka.fs :as fs]
            [cch.control.remote :as remote]
            [cch.subprocess :as subprocess]
            [cli.codex-app-server-service :as executable]
            [clojure.java.io :as io]
            [clojure.string :as str]))

(def linux-service-name "cch-control-runner.service")
(def macos-label "com.cch.control-runner")

(defn host-os [os-name]
  (let [value (str/lower-case (or os-name ""))]
    (cond
      (str/includes? value "linux") :linux
      (str/includes? value "mac") :macos
      :else :unsupported)))

(defn- escape-systemd [value]
  (-> (str value)
      (str/replace "\\" "\\\\")
      (str/replace "\"" "\\\"")
      (str/replace "%" "%%")))

(defn- escape-xml [value]
  (-> (str value)
      (str/replace "&" "&amp;")
      (str/replace "<" "&lt;")
      (str/replace ">" "&gt;")
      (str/replace "\"" "&quot;")
      (str/replace "'" "&apos;")))

(defn render-systemd [cch-bin path pairing-path]
  (-> (io/resource "service/cch-control-runner.service.template")
      slurp
      (str/replace "{{CCH_BIN}}" (escape-systemd cch-bin))
      (str/replace "{{PATH}}" (escape-systemd path))
      (str/replace "{{PAIRING_PATH}}" (escape-systemd pairing-path))))

(defn render-launchd [cch-bin home path pairing-path]
  (-> (io/resource "service/com.cch.control-runner.plist.template")
      slurp
      (str/replace "{{CCH_BIN}}" (escape-xml cch-bin))
      (str/replace "{{HOME}}" (escape-xml home))
      (str/replace "{{PATH}}" (escape-xml path))
      (str/replace "{{PAIRING_PATH}}" (escape-xml pairing-path))))

(defn service-path [os home]
  (case os
    :linux (str home "/.config/systemd/user/" linux-service-name)
    :macos (str home "/Library/LaunchAgents/" macos-label ".plist")
    nil))

(defn- checked-command! [run-command argv]
  (let [{:keys [exit err]} (run-command argv)]
    (when-not (zero? exit)
      (throw (ex-info (str "Command failed: " (str/join " " argv)
                           (when-not (str/blank? err)
                             (str ": " (str/trim err))))
                      {:type :control-runner-service-command-failed
                       :command argv :exit exit})))))

(defn- command-succeeds? [run-command argv]
  (zero? (:exit (run-command argv))))

(defn- write-service! [path contents]
  (let [directory (fs/parent path)]
    (fs/create-dirs directory)
    (let [temporary (str (fs/create-temp-file
                           {:dir directory :prefix ".cch-control-runner-"}))]
      (try
        (spit temporary contents)
        (fs/move temporary path {:replace-existing true})
        (finally
          (fs/delete-if-exists temporary))))))

(defn- install-linux!
  [{:keys [home path pairing-path cch-bin run-command resolve-command]}]
  (let [systemctl (or (resolve-command "systemctl" path)
                      (throw (ex-info "Cannot supervise control runner: systemctl is not on PATH"
                                      {:type :systemctl-unavailable})))
        unit-path (service-path :linux home)
        unit (render-systemd cch-bin path pairing-path)
        changed? (not= unit (when (fs/exists? unit-path) (slurp unit-path)))
        command #(into [systemctl "--user"] %)
        active? (command-succeeds?
                  run-command (command ["is-active" "--quiet" linux-service-name]))
        enabled? (command-succeeds?
                   run-command (command ["is-enabled" "--quiet" linux-service-name]))]
    (when changed?
      (write-service! unit-path unit)
      (checked-command! run-command (command ["daemon-reload"])))
    (when-not enabled?
      (checked-command! run-command (command ["enable" linux-service-name])))
    (when-not active?
      (checked-command! run-command (command ["start" linux-service-name])))
    {:status (cond
               (and active? changed?) :updated-restart-required
               active? :unchanged
               changed? :installed
               :else :started)
     :path unit-path :service linux-service-name :cch-bin cch-bin}))

(defn- install-macos!
  [{:keys [home path pairing-path cch-bin run-command resolve-command uid]}]
  (let [launchctl (or (resolve-command "launchctl" path)
                      (throw (ex-info "Cannot supervise control runner: launchctl is not on PATH"
                                      {:type :launchctl-unavailable})))
        plist-path (service-path :macos home)
        plist (render-launchd cch-bin home path pairing-path)
        changed? (not= plist (when (fs/exists? plist-path) (slurp plist-path)))
        domain (str "gui/" uid)
        target (str domain "/" macos-label)
        active? (command-succeeds? run-command [launchctl "print" target])]
    (when changed?
      (write-service! plist-path plist))
    (fs/create-dirs (str home "/.local/share/cch"))
    (when-not active?
      (checked-command! run-command [launchctl "bootstrap" domain plist-path]))
    {:status (cond
               (and active? changed?) :updated-restart-required
               active? :unchanged
               changed? :installed
               :else :started)
     :path plist-path :service macos-label :cch-bin cch-bin}))

(defn install!
  "Install/start the runner service on Linux or macOS. All boundaries are
  injectable for tests. The caller must first ensure pairing is configured."
  ([]
   (let [path (or (System/getenv "PATH") "")]
     (install! {:os-name (System/getProperty "os.name")
                :home (System/getProperty "user.home")
                :path path
                :pairing-path (remote/pairing-path)
                :resolve-command executable/resolve-executable
                :run-command subprocess/run
                :uid (when (= :macos (host-os (System/getProperty "os.name")))
                       (str/trim (:out (subprocess/run ["id" "-u"]))))})))
  ([{:keys [os-name path resolve-command] :as environment}]
   (let [os (host-os os-name)]
     (if (= :unsupported os)
       {:status :unsupported :reason :requires-systemd-or-launchd}
       (let [cch-bin (or (resolve-command "cch" path)
                         (throw (ex-info "Cannot supervise control runner: cch is not on PATH"
                                         {:type :cch-cli-unavailable})))
             values (assoc environment
                           :cch-bin cch-bin
                           :pairing-path (or (:pairing-path environment)
                                             (remote/pairing-path)))]
         (case os
           :linux (install-linux! values)
           :macos (install-macos! values)))))))

(defn status
  "Return a sanitized supervision status without reading pairing credentials."
  ([]
   (let [os-name (System/getProperty "os.name")
         os (host-os os-name)
         path (or (System/getenv "PATH") "")]
     (status {:os-name os-name :path path
              :resolve-command executable/resolve-executable
              :run-command subprocess/run
              :uid (when (= :macos os)
                     (str/trim (:out (subprocess/run ["id" "-u"]))))})))
  ([{:keys [os-name path resolve-command run-command uid]}]
   (case (host-os os-name)
     :linux
     (if-let [systemctl (resolve-command "systemctl" path)]
       {:status (if (command-succeeds?
                      run-command [systemctl "--user" "is-active" "--quiet"
                                   linux-service-name])
                  :active :inactive)
        :service linux-service-name}
       {:status :unavailable :reason :systemctl-unavailable})

     :macos
     (if-let [launchctl (resolve-command "launchctl" path)]
       {:status (if (command-succeeds?
                      run-command [launchctl "print"
                                   (str "gui/" uid "/" macos-label)])
                  :active :inactive)
        :service macos-label}
       {:status :unavailable :reason :launchctl-unavailable})

     {:status :unsupported :reason :requires-systemd-or-launchd})))
