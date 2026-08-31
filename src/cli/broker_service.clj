(ns cli.broker-service
  "cch install-broker-service / uninstall-broker-service — manage the systemd
  user unit that runs `cch control broker` from the self-contained runtime.

  This is the generic, repo-shipped deploy primitive. Broker configuration
  (Postgres, runner tokens, optional web switchboard) is supplied through an
  EnvironmentFile the operator provides — see
  resources/cch-control-broker.env.example. Environment-specific wrappers (a
  private monorepo, a container/Quadlet) layer on top of this.

  Brokers run on always-on hosts, so only Linux (systemd user units) is
  supported here; the containerized path (resources/cch-control-broker.container)
  covers server deploys that prefer podman."
  (:require [babashka.fs :as fs]
            [clojure.java.io :as io]
            [clojure.string :as str]))

(def ^:private unit-name "cch-control-broker.service")
(def ^:private default-host "127.0.0.1")
(def ^:private default-port 8787)

(defn- home-dir []
  (System/getProperty "user.home"))

(defn- linux? []
  (str/includes? (str/lower-case (System/getProperty "os.name")) "linux"))

(defn- runtime-bin []
  (str (home-dir) "/.local/share/cch/runtime/bin/cch"))

(defn- unit-path []
  (str (home-dir) "/.config/systemd/user/" unit-name))

(defn- env-file []
  (str (home-dir) "/.config/cch-control-broker.env"))

(defn render-unit
  "Render the broker unit for a loopback host and port. Pure for tests."
  [host port]
  (-> (io/resource "service/cch-control-broker.service.template")
      slurp
      (str/replace "{{HOST}}" (str host))
      (str/replace "{{PORT}}" (str port))))

(defn- preflight!
  "Ensure the runtime is deployed before writing a unit that references it."
  []
  (when-not (fs/exists? (runtime-bin))
    (binding [*out* *err*]
      (println "Error: cch runtime missing at" (runtime-bin))
      (println "Run `just build && just install` first to deploy it."))
    (System/exit 1)))

(defn run
  "cch install-broker-service — write the systemd user unit for the broker and
  print activation guidance. Does not start the service (config must be in
  place first)."
  [options _arguments]
  (when-not (linux?)
    (binding [*out* *err*]
      (println "Error: `cch install-broker-service` supports Linux (systemd user units).")
      (println "For macOS/dev, run `cch control broker` directly; for servers, use the")
      (println "container path (resources/cch-control-broker.container).")
      (println "Detected OS:" (System/getProperty "os.name")))
    (System/exit 1))
  (preflight!)
  (let [host (or (:host options) default-host)
        port (or (:port options) default-port)
        path (unit-path)]
    (fs/create-dirs (fs/parent path))
    (spit path (render-unit host port))
    (println (format "Installed systemd user unit at %s" path))
    (println (format "  broker listener: %s:%s" host port))
    (when-not (fs/exists? (env-file))
      (println)
      (println "NOTE: broker config not found at" (env-file))
      (println "  Copy resources/cch-control-broker.env.example there and fill it in")
      (println "  (Postgres, runner tokens, optional web switchboard) before starting."))
    (println)
    (println "Activate:    systemctl --user enable --now cch-control-broker")
    (println "Logs:        journalctl --user -u cch-control-broker -f")
    (println "Disable:     systemctl --user disable --now cch-control-broker")))

(defn run-uninstall
  "cch uninstall-broker-service — remove the unit and print the disable command."
  [_options _arguments]
  (let [path (unit-path)]
    (if (fs/exists? path)
      (do
        (fs/delete path)
        (println (format "Removed systemd user unit at %s" path))
        (println)
        (println "Stop the running service (if active):")
        (println "    systemctl --user disable --now cch-control-broker"))
      (println "No installed broker unit found at" path))))
