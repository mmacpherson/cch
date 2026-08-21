(ns cch.settings
  "Central reader for server-side runtime tunables from the global
  `~/.config/cch/config.yaml`.

  Kept dependency-light (YAML + fs only, no cch.* requires) so any module —
  including ones on the write hot path — can read a setting without forming a
  load cycle. This is the same reason cch.federation reads the global config
  directly rather than through cch.config (whose config-db → log chain would
  cycle). New server tunables belong here, one accessor per knob, each with a
  sane default so a missing or malformed config never breaks the server.

  Config shape:

    forecast:
      refresh-interval-seconds: 30   # how often the statusline forecast recomputes"
  (:require [babashka.fs :as fs]
            [clj-yaml.core :as yaml]))

(defn global-config-path
  "Path to the global cch config, honoring XDG_CONFIG_HOME."
  []
  (str (or (System/getenv "XDG_CONFIG_HOME")
           (str (System/getProperty "user.home") "/.config"))
       "/cch/config.yaml"))

(defn load-global-config
  "Parsed global config map, or nil if missing/malformed. Never throws — a bad
  config must not crash the server; callers fall back to defaults."
  []
  (let [path (global-config-path)]
    (when (fs/exists? path)
      (try (yaml/parse-string (slurp path) :keywords true)
           (catch Exception _ nil)))))

;; --- Forecast ---

(def ^:const default-forecast-refresh-seconds
  "How often the statusline forecast recomputes when new snapshots have
  arrived. The forecast is a 5h/7d burn projection, so a cadence this coarse
  is imperceptible while capping the recompute cost regardless of event rate."
  30)

(def ^:const min-forecast-refresh-seconds 5)

(defn forecast-refresh-interval-ms
  "Forecast recompute cadence in milliseconds, from `forecast.refresh-interval-seconds`
  (floored at min-forecast-refresh-seconds), defaulting when unset."
  ([] (forecast-refresh-interval-ms (load-global-config)))
  ([cfg]
   (let [secs (or (get-in cfg [:forecast :refresh-interval-seconds])
                  default-forecast-refresh-seconds)]
     (* 1000 (max min-forecast-refresh-seconds secs)))))
