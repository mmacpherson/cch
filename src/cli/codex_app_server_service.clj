(ns cli.codex-app-server-service
  "Install the Linux systemd user service for Codex's shared app-server.

  This deliberately runs the package-managed Codex binary directly. It does
  not use Codex's experimental standalone daemon manager, updater loop, or
  proprietary Remote Control transport."
  (:require [babashka.fs :as fs]
            [cch.control.unix-websocket :as websocket]
            [cch.subprocess :as subprocess]
            [cli.codex-settings :as codex-settings]
            [clojure.java.io :as io]
            [clojure.string :as str])
  (:import [java.io File]
           [java.util.regex Pattern]))

(def service-name "cch-codex-app-server.service")

(defn linux?
  "True when os-name identifies Linux. Pure for tests."
  [os-name]
  (str/includes? (str/lower-case (or os-name "")) "linux"))

(defn resolve-executable
  "Resolve command against a PATH string without invoking a shell."
  [command path]
  (let [direct (File. command)]
    (if (str/includes? command File/separator)
      (when (and (.isFile direct) (.canExecute direct))
        (.getAbsolutePath direct))
      (some (fn [directory]
              (let [candidate (File. directory command)]
                (when (and (.isFile candidate) (.canExecute candidate))
                  (.getAbsolutePath candidate))))
            (remove str/blank?
                    (str/split (or path "")
                               (re-pattern
                                 (Pattern/quote File/pathSeparator))))))))

(defn- escape-systemd-value [value]
  (-> (str value)
      (str/replace "\\" "\\\\")
      (str/replace "\"" "\\\"")
      ;; A literal percent must not be interpreted as a systemd specifier.
      (str/replace "%" "%%")))

(defn render-unit
  "Render a unit for an absolute Codex binary, CODEX_HOME, and user PATH."
  [codex-bin codex-home path]
  (-> (io/resource "service/cch-codex-app-server.service.template")
      slurp
      (str/replace "{{CODEX_BIN}}" (escape-systemd-value codex-bin))
      (str/replace "{{CODEX_HOME}}" (escape-systemd-value codex-home))
      (str/replace "{{PATH}}" (escape-systemd-value path))))

(defn service-path [home]
  (str home "/.config/systemd/user/" service-name))

(defn socket-path [codex-home]
  (str codex-home "/app-server-control/app-server-control.sock"))

(defn- wait-ready!
  "Wait up to ten seconds for a complete WebSocket upgrade, not just a socket file."
  [path]
  (loop [attempt 0
         last-error nil]
    (if (>= attempt 200)
      (throw (ex-info (str "Codex app-server did not become ready at " path
                           (when last-error
                             (str ": " (.getMessage ^Exception last-error))))
                      {:type :codex-service-readiness-timeout
                       :socket-path path}
                      last-error))
      (let [result (try
                     (let [connection (websocket/connect! path)]
                       (websocket/close! connection)
                       :ready)
                     (catch Exception error error))]
        (if (= :ready result)
          true
          (do
            (Thread/sleep 50)
            (recur (inc attempt) result)))))))

(defn- checked-command! [run-command argv]
  (let [{:keys [exit err]} (run-command argv)]
    (when-not (zero? exit)
      (throw (ex-info (str "Command failed: " (str/join " " argv)
                           (when-not (str/blank? err)
                             (str ": " (str/trim err))))
                      {:type :codex-service-command-failed
                       :command argv
                       :exit exit})))))

(defn- command-succeeds? [run-command argv]
  (zero? (:exit (run-command argv))))

(defn- write-unit! [path contents]
  (let [directory (fs/parent path)]
    (fs/create-dirs directory)
    (let [temporary (str (fs/create-temp-file
                           {:dir directory :prefix ".cch-codex-app-server-"}))]
      (try
        (spit temporary contents)
        (fs/move temporary path {:replace-existing true})
        (finally
          (fs/delete-if-exists temporary))))))

(defn install!
  "Write, enable, and start the systemd user service without disrupting it.

  An already-active service is never implicitly restarted. If its unit changes,
  the new definition is installed and reported as requiring a deliberate later
  restart; attached Codex clients keep their current app-server connection.

  The optional environment map makes all host/process boundaries testable.
  Returns :unsupported outside Linux instead of changing another platform."
  ([]
   (install! {:os-name (System/getProperty "os.name")
              :home (System/getProperty "user.home")
              :codex-home (codex-settings/codex-home)
              :path (System/getenv "PATH")
              :resolve-command resolve-executable
              :run-command subprocess/run
              :wait-ready wait-ready!}))
  ([{:keys [os-name home codex-home path resolve-command run-command wait-ready]}]
   (if-not (linux? os-name)
     {:status :unsupported :reason :requires-linux-systemd}
     (let [codex-bin (or (resolve-command "codex" path)
                         (throw (ex-info "Cannot install Codex app-server service: codex is not on PATH"
                                         {:type :codex-cli-unavailable})))
           systemctl-bin (or (resolve-command "systemctl" path)
                             (throw (ex-info "Cannot install Codex app-server service: systemctl is not on PATH"
                                             {:type :systemctl-unavailable})))
           unit-path (service-path home)
           unit (render-unit codex-bin codex-home path)
           changed? (not= unit (when (fs/exists? unit-path) (slurp unit-path)))
           systemctl #(into [systemctl-bin "--user"] %)
           active? (command-succeeds?
                     run-command (systemctl ["is-active" "--quiet" service-name]))
           enabled? (command-succeeds?
                      run-command (systemctl ["is-enabled" "--quiet" service-name]))]
       (when changed?
         (write-unit! unit-path unit)
         (checked-command! run-command (systemctl ["daemon-reload"])))
       (when-not enabled?
         (checked-command! run-command (systemctl ["enable" service-name])))
       (when-not active?
         (checked-command! run-command (systemctl ["start" service-name])))
       (wait-ready (socket-path codex-home))
       {:status (cond
                  (and active? changed?) :updated-restart-required
                  active? :unchanged
                  changed? :installed
                  :else :started)
        :path unit-path
        :service service-name
        :codex-bin codex-bin
        :codex-home codex-home}))))
