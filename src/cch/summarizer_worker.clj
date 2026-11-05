(ns cch.summarizer-worker
  "Background summarizer: after each completed turn, ask a small model for a
  one-line status and a short role name for the session.

  Opt-in per runner through the global config:

    summarizer:
      enabled: true
      daily-usd-cap: 1.0          # stop calling for the rest of the UTC day
      model: claude-haiku-5-5     # the only priced model; others are refused
      key-file: ~/.local/share/cch/anthropic-api-key
      known-hosts:                # machine names to redact, with optional labels
        myhost: this-runner

  Only a scrubbed digest of the turn (prompt, one-line tool summaries, the
  closing reply) and other sessions' already-scrubbed summaries leave the
  machine. The model's answer only fills display fields."
  (:require [babashka.fs :as fs]
            [cch.activity-trace :as activity-trace]
            [cch.config :as config]
            [cch.control.store :as store]
            [cch.log :as log]
            [cch.scrub :as scrub]
            [cch.summarizer :as summarizer]
            [cch.summarizer-client :as client]
            [clojure.string :as str])
  (:import [java.time LocalDate LocalDateTime ZoneOffset]
           [java.time.format DateTimeFormatter]))

(def priced-models
  "Models cch.summarizer/cost-usd prices correctly. Others are refused so the
  daily cap cannot under-count a pricier model."
  #{"claude-haiku-5-5"})

(def ^:private defaults
  {:enabled false
   :daily-usd-cap 1.0
   :model "claude-haiku-5-5"
   :key-file client/default-key-file
   :poll-ms 20000
   :fleet-window-ms (* 24 60 60 1000)
   :seed-window-hours 2})

(defn- expand-home [path]
  (if (and (string? path) (str/starts-with? path "~/"))
    (str (System/getProperty "user.home") (subs path 1))
    path))

(defn settings
  "Effective summarizer settings from the global config. Pure given cfg."
  [cfg]
  (let [s (merge defaults (:summarizer cfg))]
    (-> s
        (update :key-file expand-home)
        (update :known-hosts #(into {} (map (fn [[k v]] [(name k) v])) %)))))

(defn- load-settings []
  (settings (try (config/load-yaml (config/global-config-path))
                 (catch Exception _ nil))))

(defn route-id [agent session-id]
  (str (if (= "codex" agent) "codex:" "claude:") session-id))

(defn- today-utc [] (str (LocalDate/now ZoneOffset/UTC)))

(defn- seed-cursor [hours]
  (log/first-event-id-since
    (.format (.minusHours (LocalDateTime/now ZoneOffset/UTC) hours)
             (DateTimeFormatter/ofPattern "yyyy-MM-dd'T'HH:mm:ss.SSS"))))

(defn fleet-context
  "Other sessions' summaries updated within window-ms, for distinct naming."
  [summaries route now-ms window-ms]
  (vec (for [[r {:keys [auto-name repo status updated-at]}] summaries
             :when (and (not= r route) updated-at
                        (< (- now-ms updated-at) window-ms))]
         {:name auto-name :repo repo :status status})))

(defn- ended-turns [rows]
  (filter #(and (:ended-at %) (or (seq (:prompts %)) (seq (:steps %))))
          (activity-trace/turns rows)))

(defn naming-input
  "Digest input for the request. A session's first name should describe its
  overall work, so an unnamed session's input spans its last few turns;
  otherwise only the latest. Pure."
  [turns named?]
  (let [chosen (if named? [(last turns)] (take-last 3 turns))
        inputs (map activity-trace/digest-input chosen)]
    {:prompt (str/join "\n---\n" (keep #(not-empty (:prompt %)) inputs))
     :tool-summaries (vec (mapcat :tool-summaries inputs))
     :closing-message (:closing-message (last inputs))}))

(defn summarize-session!
  "Summarize the latest completed turn of one session, unless it was already
  summarized or the daily cap is reached. Returns a keyword outcome."
  [{:keys [model known-hosts daily-usd-cap fleet-window-ms]} api-key agent session-id]
  (let [rows (log/session-trace-rows session-id :limit 600)
        turns (ended-turns rows)
        turn (last turns)
        route (route-id agent session-id)
        summaries (store/session-summaries)
        current (get summaries route)
        spent (:cost_usd (store/summarizer-usage (today-utc)))]
    (cond
      (nil? turn) :no-turn
      (= (:turn-key turn) (:turn-key current)) :unchanged
      (not (summarizer/within-budget? spent daily-usd-cap)) :over-budget
      :else
      (let [scrub-opts {:known-hosts known-hosts}
            repo (some-> (:cwd (last rows)) fs/file-name str (scrub/scrub scrub-opts) :text)
            digest (:text (scrub/digest (naming-input turns (some? (:auto-name current)))
                                        (assoc scrub-opts :max-chars 6000)))
            fleet (fleet-context summaries route (System/currentTimeMillis) fleet-window-ms)
            {:keys [system user schema]} (summarizer/request
                                           {:digest digest
                                            :current {:name (:auto-name current)
                                                      :status (:status current)}
                                            :fleet fleet
                                            :repo repo})
            resp (client/call! {:api-key api-key :model model :system system
                                :user user :schema-map schema :max-tokens 1024})
            cost (double (summarizer/cost-usd (select-keys resp [:input-tokens :output-tokens])))
            _ (store/record-summarizer-usage! (today-utc) (assoc resp :cost-usd cost))
            proposal (summarizer/parse-response (:text resp) scrub-opts)]
        (if-not proposal
          :invalid-response
          (let [taken (keep :name fleet)
                renaming? (or (nil? (:auto-name current)) (:topic-changed proposal))
                proposal (cond-> proposal
                           renaming? (update :name summarizer/unique-name taken))
                state (summarizer/next-state {:name (:auto-name current)
                                              :status (:status current)}
                                             proposal {:alias nil})]
            (store/save-session-summary! {:route-id route :status (:status state)
                                          :auto-name (:name state) :repo repo
                                          :turn-key (:turn-key turn)})
            :summarized))))))

(defn tick!
  "One pass: summarize each session whose turn ended since the cursor."
  []
  (let [{:keys [enabled key-file seed-window-hours model] :as s} (load-settings)
        api-key (when (and enabled (contains? priced-models model))
                  (client/read-key key-file))]
    (when api-key
      (let [cursor (or (store/summarizer-cursor) (seed-cursor seed-window-hours))
            ends (log/turn-ends-after cursor)
            sessions (distinct (map (juxt :agent :session_id) ends))]
        (doseq [[agent sid] sessions]
          (try
            (summarize-session! s api-key agent sid)
            (catch Exception e
              (println (format "cch summarizer: %s session skipped (%s)"
                               agent (.getSimpleName (class e)))))))
        (when-let [last-id (:id (last ends))]
          (store/save-summarizer-cursor! last-id))))))

(defonce ^:private worker (atom nil))

(defn start!
  "Start the polling worker. It checks the config on every pass, so enabling
  or disabling takes effect without a restart."
  []
  (when-not @worker
    (let [running (atom true)
          t (Thread.
              (fn []
                (while @running
                  (try (tick!) (catch Throwable e
                                 (println "cch summarizer: pass failed:"
                                          (.getSimpleName (class e)))))
                  (try (Thread/sleep (long (:poll-ms (load-settings))))
                       (catch InterruptedException _ (reset! running false)))))
              "cch-summarizer")]
      (.setDaemon t true)
      (.start t)
      (reset! worker {:running running :thread t}))))

(defn stop! []
  (when-let [{:keys [running ^Thread thread]} @worker]
    (reset! running false)
    (.interrupt thread)
    (reset! worker nil)))
