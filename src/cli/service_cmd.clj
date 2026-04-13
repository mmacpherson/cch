(ns cli.service-cmd
  "cch install-service / uninstall-service — manages the OS-native
  auto-start unit (systemd user unit on Linux, launchd LaunchAgent on
  macOS) that keeps `cch serve` running across reboots and crashes.

  We generate the unit/plist from a template and write it to the
  canonical location; activation is left to the user to run explicitly
  so nothing starts silently."
  (:require [babashka.fs :as fs]
            [babashka.process :as p]
            [clojure.java.io :as io]
            [clojure.string :as str]))

(defn- host-os
  "Classify the current OS via `os.name`. Returns :linux, :macos, or
  :unsupported."
  []
  (let [n (str/lower-case (System/getProperty "os.name"))]
    (cond
      (str/includes? n "linux") :linux
      (str/includes? n "mac")   :macos
      :else                      :unsupported)))

(defn- home-dir []
  (System/getProperty "user.home"))

(defn- cch-repo-symlink
  "Absolute path to the cch repo symlink that `cch init` creates."
  []
  (str (home-dir) "/.local/share/cch/repo"))

(def ^:private uid
  "Memoized user UID — used for macOS `launchctl bootstrap gui/<uid>`."
  (delay
    (try
      (-> (p/sh ["id" "-u"]) :out str/trim)
      (catch Exception _ ""))))

;; --- Platform-specific descriptors ---

(defmulti ^:private platform-info
  "Returns {:label :path :template :activate :disable :logs} for the
  current host OS."
  identity)

(defmethod platform-info :linux [_]
  {:label    "systemd user unit"
   :path     (str (home-dir) "/.config/systemd/user/cch.service")
   :template "service/cch.service.template"
   :activate "systemctl --user enable --now cch"
   :disable  "systemctl --user disable --now cch"
   :logs     "journalctl --user -u cch -f"})

(defmethod platform-info :macos [_]
  {:label    "LaunchAgent"
   :path     (str (home-dir) "/Library/LaunchAgents/com.cch.server.plist")
   :template "service/com.cch.server.plist.template"
   :activate (str "launchctl bootstrap gui/" @uid " "
                  (home-dir) "/Library/LaunchAgents/com.cch.server.plist")
   :disable  (str "launchctl bootout gui/" @uid " "
                  (home-dir) "/Library/LaunchAgents/com.cch.server.plist")
   :logs     (str "tail -f " (home-dir) "/.local/share/cch/serve.log")})

;; --- Template rendering ---

(defn- render-template
  "Read the classpath resource and substitute {{HOME}} with the user's
  home dir. systemd uses its own %h specifier natively so only the
  launchd template actually uses {{HOME}}, but we run the substitution
  unconditionally — it's harmless on the systemd template."
  [resource-path]
  (-> (io/resource resource-path)
      slurp
      (str/replace "{{HOME}}" (home-dir))))

;; --- Preconditions ---

(defn- preflight!
  "Verifies the cch repo symlink exists. Aborts with a helpful error if
  not, so install-service never writes a broken unit."
  []
  (when-not (fs/exists? (cch-repo-symlink))
    (binding [*out* *err*]
      (println "Error: cch repo symlink missing at" (cch-repo-symlink))
      (println "Run `cch init` first to create it."))
    (System/exit 1)))

;; --- Commands ---

(defn run
  "cch install-service — write the unit/plist for the current OS and
  print the activation command."
  [& _args]
  (let [os (host-os)]
    (when (= os :unsupported)
      (binding [*out* *err*]
        (println "Error: `cch install-service` only supports Linux (systemd user units)")
        (println "and macOS (launchd LaunchAgents) right now.")
        (println "Detected OS:" (System/getProperty "os.name")))
      (System/exit 1))
    (preflight!)
    (let [{:keys [label path template activate disable logs]} (platform-info os)
          rendered (render-template template)]
      (fs/create-dirs (fs/parent path))
      ;; macOS log path is referenced in the plist; make sure the dir
      ;; exists so launchd doesn't fail to open the log at startup.
      (when (= os :macos)
        (fs/create-dirs (str (home-dir) "/.local/share/cch")))
      (spit path rendered)
      (println (format "Installed %s at %s" label path))
      (println)
      (println (format "Activate:    %s" activate))
      (println (format "Logs:        %s" logs))
      (println (format "Disable:     %s" disable)))))

(defn run-uninstall
  "cch uninstall-service — remove the unit/plist and print the disable
  command the user should run to stop the service."
  [& _args]
  (let [os (host-os)]
    (when (= os :unsupported)
      (binding [*out* *err*]
        (println "Error: unsupported OS:" (System/getProperty "os.name")))
      (System/exit 1))
    (let [{:keys [label path disable]} (platform-info os)]
      (if (fs/exists? path)
        (do
          (fs/delete path)
          (println (format "Removed %s at %s" label path))
          (println)
          (println "Disable running service (if active):")
          (println (format "    %s" disable)))
        (println "No installed service found at" path)))))
