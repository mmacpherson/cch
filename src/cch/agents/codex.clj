(ns cch.agents.codex
  "Codex adapter — translates the cch hook registry into Codex
  `~/.codex/config.toml` entries.

  Codex hooks are command-type entries (no native HTTP support; see the
  shim discussion in claude-code-hooks-bp0). Each registered event gets a
  curl-to-the-dispatcher command. All entries live inside a single
  sentinel-tagged block so `uninstall` can strip them surgically.

  See `cli.codex-settings` for the underlying read/write primitives."
  (:require [cli.codex-settings :as cs]
            [cli.registry :as registry]))

(def ^:const block-name
  "Single cch-managed sentinel block in Codex config — all dispatcher
  events live inside it so uninstall is a one-shot strip."
  "cch-dispatcher")

(def ^:const default-host "127.0.0.1")
(def ^:const default-port 8888)
(def ^:const default-timeout 30)

(defn dispatch-command
  "Render the curl command Codex will run for `event`. Sends Codex's
  stdin payload (Claude-compatible JSON) to the cch dispatcher."
  [event & {:keys [host port] :or {host default-host port default-port}}]
  (format "curl -s -X POST --data-binary @- http://%s:%d/dispatch/%s"
          host port event))

(defn registry->entries
  "Convert the cch registry's dispatcher-events into entries the codex
  settings writer expects. Pure: takes no I/O."
  [dispatcher-events & {:keys [host port timeout]
                        :or {host default-host
                             port default-port
                             timeout default-timeout}}]
  (mapv (fn [{:keys [event matcher]}]
          {:event   event
           :matcher (or matcher ".*")
           :command (dispatch-command event :host host :port port)
           :timeout timeout})
        dispatcher-events))

(defn install!
  "Write all cch dispatcher entries into the user's Codex config.

  Idempotent — replaces any prior cch-managed block. Leaves user-written
  TOML untouched. Returns the path written."
  [& {:keys [config-path host port timeout]}]
  (registry/validate-registry!)
  (let [path     (or config-path (cs/codex-config-path))
        entries  (registry->entries (registry/dispatcher-events)
                                    :host (or host default-host)
                                    :port (or port default-port)
                                    :timeout (or timeout default-timeout))]
    (cs/install-hook! path block-name entries)
    path))

(defn uninstall!
  "Strip the cch-managed block from the user's Codex config. Leaves any
  user-written hooks untouched."
  [& {:keys [config-path]}]
  (let [path (or config-path (cs/codex-config-path))]
    (cs/remove-hook! path block-name)
    path))
