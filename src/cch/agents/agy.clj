(ns cch.agents.agy
  "Antigravity CLI (agy) usage adapter.

  AGY's lifecycle hooks carry tool/event data, but quota is exposed through
  its statusLine JSON payload. This namespace normalizes that payload into
  cch's canonical Claude-shaped `rate_limits` map and owns installation of a
  lightweight status-line forwarding script."
  (:require [babashka.fs :as fs]
            [cheshire.core :as json]
            [cli.settings :as settings]
            [clojure.java.io :as io]
            [clojure.string :as str])
  (:import (java.time Instant)))

(def ^:const default-host "127.0.0.1")
(def ^:const default-port 8888)

(defn settings-path
  "Default AGY CLI settings file."
  []
  (str (System/getProperty "user.home")
       "/.gemini/antigravity-cli/settings.json"))

(defn script-path
  "Default durable location for cch's AGY status-line adapter."
  []
  (str (System/getProperty "user.home")
       "/.local/share/cch/agy-statusline-command.sh"))

(defn backup-path
  "Sidecar used to restore the pre-cch statusLine configuration."
  []
  (str (System/getProperty "user.home")
       "/.gemini/antigravity-cli/cch-statusline-backup.json"))

(defn hooks-config-path
  "AGY's GLOBAL lifecycle-hooks file. Per the Antigravity customization guide,
  global discovery is `~/.gemini/config/`, so a hooks.json there applies to
  every agy session regardless of workspace."
  []
  (str (System/getProperty "user.home") "/.gemini/config/hooks.json"))

(defn- bucket-window
  "Map an AGY quota bucket ID onto a cch window. AGY currently documents a
  `gemini-weekly` bucket. Explicit 5h aliases are accepted for forward
  compatibility; unknown periods are deliberately ignored rather than
  plotted against a fabricated window."
  [bucket-id]
  (let [s (str/lower-case (name bucket-id))]
    (cond
      (re-find #"(weekly|week|7[-_]?d)" s) :seven_day
      (re-find #"(five[-_]?hour|5[-_]?h)" s) :five_hour
      :else nil)))

(defn- reset-epoch
  [{:keys [reset_time reset_in_seconds]} now-epoch]
  (or (when (string? reset_time)
        (try
          (.getEpochSecond (Instant/parse reset_time))
          (catch Exception _ nil)))
      (when (number? reset_in_seconds)
        (+ (long now-epoch) (long reset_in_seconds)))))

(defn- quota-limit
  [quota now-epoch]
  (let [remaining (:remaining_fraction quota)
        resets-at (reset-epoch quota now-epoch)]
    (when (and (number? remaining) resets-at)
      (let [fraction (-> (double remaining) (max 0.0) (min 1.0))]
        {:used_percentage (* 100.0 (- 1.0 fraction))
         :resets_at       resets-at}))))

(defn agy-rate-limits
  "Convert AGY's model/bucket quota map to cch's canonical rate-limit map.

  If several buckets describe the same window, retain the most-consumed
  bucket. That makes the single AGY chart conservatively represent the quota
  closest to exhaustion."
  ([quota] (agy-rate-limits quota (.getEpochSecond (Instant/now))))
  ([quota now-epoch]
   (reduce-kv
     (fn [limits bucket-id bucket]
       (if-let [window (bucket-window bucket-id)]
         (if-let [candidate (quota-limit bucket now-epoch)]
           (let [current (get limits window)]
             (if (or (nil? current)
                     (> (:used_percentage candidate)
                        (:used_percentage current)))
               (assoc limits window candidate)
               limits))
           limits)
         limits))
     {}
     (or quota {}))))

(defn normalize-status-payload
  "Add canonical `rate_limits` to an AGY statusLine payload. The original
  AGY fields remain intact for diagnostics and future schema evolution."
  ([payload] (normalize-status-payload payload (.getEpochSecond (Instant/now))))
  ([payload now-epoch]
   (let [normalized (agy-rate-limits (:quota payload) now-epoch)]
     (if (seq normalized)
       (assoc payload :rate_limits
              (merge (:rate_limits payload) normalized))
       payload))))

(def ^:const capture-heartbeat-ms
  "Maximum interval between stored snapshots while AGY is emitting an
  unchanged status line."
  30000)

(def ^:private capture-state-ttl-ms
  (* 24 60 60 1000))

(def ^:private max-capture-sessions 256)

(defonce ^:private capture-state
  (atom {}))

(defn reset-capture-state!
  "Clear in-memory AGY coalescing state. Primarily useful for tests and
  live-reload sessions."
  []
  (reset! capture-state {}))

(defn- capture-session-key
  [payload]
  (or (:session_id payload)
      (:conversation_id payload)))

(defn- trim-capture-state
  [state now-ms]
  (let [fresh (into {}
                    (filter (fn [[_ {:keys [last-seen-at]}]]
                              (< (- now-ms last-seen-at)
                                 capture-state-ttl-ms)))
                    state)]
    (if (> (count fresh) max-capture-sessions)
      (->> fresh
           (sort-by (comp :last-seen-at val) >)
           (take max-capture-sessions)
           (into {}))
      fresh)))

(defn should-capture?
  "Atomically decide whether an AGY status payload should create a DB row.

  Capture the first payload, every quota change, transitions into `idle`
  (which carry the final context totals), and a 30-second heartbeat. Repeated
  working-state emissions between those boundaries are acknowledged but
  coalesced. Payloads without a session ID are always retained because they
  cannot be safely correlated."
  ([payload] (should-capture? payload (System/currentTimeMillis)))
  ([payload now-ms]
   (if-let [session-key (capture-session-key payload)]
     (let [[before after]
           (swap-vals!
             capture-state
             (fn [state]
               (let [state          (trim-capture-state state now-ms)
                     previous       (get state session-key)
                     agent-state    (:agent_state payload)
                     quota-signature (:rate_limits payload)
                     first?         (nil? previous)
                     quota-change?  (and previous
                                         (not= quota-signature
                                               (:quota-signature previous)))
                     became-idle?   (and previous
                                         (= "idle" agent-state)
                                         (not= "idle" (:last-agent-state previous)))
                     heartbeat?     (and previous
                                         (>= (- now-ms (:captured-at previous))
                                             capture-heartbeat-ms))
                     capture?       (or first?
                                        quota-change?
                                        became-idle?
                                        heartbeat?)
                     next-state     (cond-> (assoc previous
                                                   :last-seen-at now-ms
                                                   :last-agent-state agent-state)
                                      capture?
                                      (assoc :captured-at now-ms
                                             :quota-signature quota-signature
                                             :capture-count
                                             (inc (or (:capture-count previous) 0))))]
                 (assoc state session-key next-state))))
           before-count (get-in before [session-key :capture-count] 0)
           after-count  (get-in after [session-key :capture-count] 0)]
       (> after-count before-count))
     true)))

(defn- adapter-script []
  (if-let [resource (io/resource "agy-statusline-command.sh")]
    (slurp resource)
    (throw (ex-info "Bundled AGY status-line script is missing" {}))))

(defn- managed-status-line?
  [status-line adapter-path]
  (= adapter-path (:command status-line)))

(defn install!
  "Install the durable AGY status-line adapter and point AGY at it.

  The previous statusLine block is saved in a sidecar and restored by
  `uninstall!`. Re-running install is idempotent."
  [& {:keys [config-path adapter-path saved-path]}]
  (let [config-path  (or config-path (settings-path))
        adapter-path (or adapter-path (script-path))
        saved-path   (or saved-path (backup-path))
        config       (settings/read-settings config-path)
        previous     (:statusLine config)]
    (when-not (managed-status-line? previous adapter-path)
      (settings/write-settings!
        saved-path
        {:had_status_line (contains? config :statusLine)
         :status_line     previous}))
    (when-let [parent (fs/parent adapter-path)]
      (fs/create-dirs parent))
    (spit adapter-path (adapter-script))
    (.setExecutable (io/file adapter-path) true false)
    (settings/write-settings!
      config-path
      (assoc config :statusLine {:type    "command"
                                 :command adapter-path
                                 :enabled true}))
    {:path config-path
     :script adapter-path
     :backup saved-path}))

(defn uninstall!
  "Remove cch's AGY status-line adapter. Restore the previous statusLine only
  when AGY still points at the cch-managed command, preserving any user change
  made after installation."
  [& {:keys [config-path adapter-path saved-path]}]
  (let [config-path  (or config-path (settings-path))
        adapter-path (or adapter-path (script-path))
        saved-path   (or saved-path (backup-path))
        config       (settings/read-settings config-path)
        managed?     (managed-status-line? (:statusLine config) adapter-path)
        saved        (when (fs/exists? saved-path)
                       (settings/read-settings saved-path))
        restored     (and managed? (some? saved))]
    (when managed?
      (settings/write-settings!
        config-path
        (if (:had_status_line saved)
          (assoc config :statusLine (:status_line saved))
          (dissoc config :statusLine))))
    (fs/delete-if-exists adapter-path)
    (when restored
      (fs/delete-if-exists saved-path))
    {:path config-path
     :script adapter-path
     :restored restored}))

;; --- Lifecycle hooks (event federation) ---
;;
;; AGY lifecycle hooks are `type: command` only (no native HTTP), so — like
;; the Codex adapter — cch installs a curl-to-/dispatch/ command per event.
;; Hooks live in a single JSON object keyed by hook name; cch owns the
;; `cch-dispatcher` key and leaves any user hooks untouched.

(def ^:const hook-key
  "The cch-owned key in agy's hooks.json. Isolating our config under one
  named hook lets uninstall strip it without disturbing user hooks."
  "cch-dispatcher")

(defn dispatch-command
  "Render the curl command agy runs for `event`: pipe agy's stdin payload to
  the cch dispatcher, tagged X-CCH-Agent: agy so the dispatcher normalizes the
  agy-shaped payload and attributes rows. Short timeout + fail-open (-sS, empty
  stdout on failure) so a wedged/absent cch never blocks the agent loop."
  [event & {:keys [host port] :or {host default-host port default-port}}]
  (format "curl -sS --max-time 5 -H 'X-CCH-Agent: agy' --data-binary @- http://%s:%d/dispatch/%s"
          host port event))

(defn dispatcher-hooks
  "The cch-managed hooks.json fragment: {hook-key {event -> handlers}}.
  Pre/PostToolUse are the grouped shape (matcher + hooks wrapper); Stop is the
  flat shape (a bare handler list), per AGY's hooks spec. String keys so the
  serialized JSON matches AGY's schema and merges with user hooks verbatim."
  [& {:keys [host port] :or {host default-host port default-port}}]
  (let [cmd     (fn [ev] (dispatch-command ev :host host :port port))
        grouped (fn [ev] [{"matcher" "*"
                           "hooks" [{"type" "command" "command" (cmd ev) "timeout" 10}]}])
        flat    (fn [ev] [{"type" "command" "command" (cmd ev) "timeout" 10}])]
    {hook-key {"PreToolUse"  (grouped "PreToolUse")
               "PostToolUse" (grouped "PostToolUse")
               "Stop"        (flat "Stop")}}))

(defn- read-hooks-json
  "Parse agy's hooks.json (string keys preserved), or nil if absent/malformed."
  [path]
  (when (fs/exists? path)
    (try (json/parse-string (slurp path)) (catch Exception _ nil))))

(defn install-hooks!
  "Write cch's dispatcher lifecycle hooks into agy's global hooks.json under
  `hook-key`, preserving any existing user hooks. Idempotent (re-running just
  overwrites our own key)."
  [& {:keys [config-path host port]}]
  (let [path     (or config-path (hooks-config-path))
        existing (or (read-hooks-json path) {})
        merged   (merge existing (dispatcher-hooks :host (or host default-host)
                                                   :port (or port default-port)))]
    (when-let [parent (fs/parent path)]
      (fs/create-dirs parent))
    (spit path (json/generate-string merged {:pretty true}))
    {:path path :hook-key hook-key :events ["PreToolUse" "PostToolUse" "Stop"]}))

(defn uninstall-hooks!
  "Remove cch's `hook-key` from agy's hooks.json, preserving user hooks. Deletes
  the file if it becomes empty. No-op if the file is absent."
  [& {:keys [config-path]}]
  (let [path     (or config-path (hooks-config-path))
        existing (read-hooks-json path)]
    (when existing
      (let [cleaned (dissoc existing hook-key)]
        (if (empty? cleaned)
          (fs/delete-if-exists path)
          (spit path (json/generate-string cleaned {:pretty true})))))
    {:path path :removed (boolean (get existing hook-key))}))

(defn normalize-event-payload
  "Translate an AGY lifecycle-hook payload (camelCase protojson) into cch's
  canonical Claude-shaped input keys so the dispatcher's matcher and the log
  middleware read tool/session/cwd correctly. `event` is the dispatch event
  name from the URL — AGY payloads don't carry it. Original AGY fields are kept
  (they flow into the row's `extra` blob for diagnostics)."
  [event payload]
  (let [tc (:toolCall payload)]
    (cond-> (assoc payload
                   :hook_event_name event
                   :session_id (:conversationId payload)
                   :cwd (first (:workspacePaths payload)))
      tc (assoc :tool_name  (:name tc)
                :tool_input (:args tc)))))

(defn ->hook-response
  "Render AGY's expected hook stdout for `event` from cch's reconciled result
  (a map with optional :decision/:reason, or nil = allow).

  Critically, AGY reads an EMPTY PreToolUse response as a DENY, so a
  no-decision (logger-only) result must return an explicit `allow` or every
  tool call is blocked. PostToolUse and Stop take a neutral empty object —
  Stop without a `{decision:\"continue\"}` lets the agent stop normally."
  [event reconciled]
  (let [reason (:reason reconciled)]
    (if (= event "PreToolUse")
      (let [d (case (:decision reconciled)
                (:deny :block) "deny"
                :ask           "ask"
                "allow")]
        (json/generate-string (cond-> {:decision d} reason (assoc :reason reason))))
      (json/generate-string {}))))
