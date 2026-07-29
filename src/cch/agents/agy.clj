(ns cch.agents.agy
  "Antigravity CLI (agy) usage adapter.

  AGY's lifecycle hooks carry tool/event data, but quota is exposed through
  its statusLine JSON payload. This namespace normalizes that payload into
  cch's canonical Claude-shaped `rate_limits` map and owns installation of a
  lightweight status-line forwarding script."
  (:require [babashka.fs :as fs]
            [cli.settings :as settings]
            [clojure.java.io :as io]
            [clojure.string :as str])
  (:import (java.time Instant)))

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
