(ns cch.trace-page
  "Per-session activity trace for the runner-local webapp.

  Turns come from cch.activity-trace; this namespace places cch messages into
  them and renders hiccup. Rendering is pure: callers pass the data and a
  zone. The page shows prompts, commands, and agent replies, so it is served
  only by the loopback-bound dispatcher."
  (:require [cch.control.naming :as naming]
            [clojure.string :as str])
  (:import [java.time Instant LocalDateTime ZoneId ZoneOffset]
           [java.time.format DateTimeFormatter DateTimeParseException]))

;; --- Time ---

(defn event-ts->ms
  "Epoch milliseconds for an events.timestamp (UTC, no zone), or nil."
  [ts]
  (when (string? ts)
    (try
      (-> (LocalDateTime/parse ts) (.toInstant ZoneOffset/UTC) .toEpochMilli)
      (catch DateTimeParseException _ nil))))

(def ^:private clock (DateTimeFormatter/ofPattern "HH:mm:ss"))
(def ^:private day-clock (DateTimeFormatter/ofPattern "MMM d HH:mm"))

(defn- local-time [ms ^ZoneId zone fmt]
  (when ms (.format (.atZone (Instant/ofEpochMilli ms) zone) fmt)))

(defn- duration-label [ms]
  (when (and ms (not (neg? ms)))
    (let [s (quot (long ms) 1000)]
      (cond
        (< ms 1000) (str (long ms) " ms")
        (< s 60) (str s " s")
        (< s 3600) (str (quot s 60) " min " (mod s 60) " s")
        :else (str (quot s 3600) " h " (quot (mod s 3600) 60) " min")))))

;; --- Routes and names ---

(defn route-id
  "The control-plane route for a session: claude:<id> or codex:<id>."
  [agent session-id]
  (str (if (= "codex" agent) "codex:" "claude:") session-id))

(defn session-name
  "The session's alias or provider-advertised name, when known. `names` maps
  route -> {:name string-or-nil}."
  [agent session-id names]
  (get-in names [(route-id agent session-id) :name]))

(defn- name-cell
  "The session's name for display: its own name, else the model-written auto
  name (marked as such), else nil."
  [agent session-id names]
  (let [{:keys [name auto-name]} (get names (route-id agent session-id))]
    (cond
      name [:span name]
      auto-name [:span.trace-auto {:title "name suggested by the summarizer"} auto-name]
      :else nil)))

(defn session-status [agent session-id names]
  (get-in names [(route-id agent session-id) :status]))

(defn session-label
  "Name and mnemonic together (\"homelab-a8 · rapid-maple-af69\"), or the
  mnemonic alone when the session has no name."
  ([agent session-id] (session-label agent session-id {}))
  ([agent session-id names]
   (let [route (route-id agent session-id)
         mnemonic (naming/mnemonic route)]
     (if-let [n (get-in names [route :name])] (str n " · " mnemonic) mnemonic))))

(defn- peer-label [peer names]
  (cond
    (= "operator" peer) "operator"
    :else (let [mnemonic (naming/mnemonic peer)]
            (if-let [n (get-in names [peer :name])] (str n " · " mnemonic) mnemonic))))

;; --- Messages ---

(defn message-step
  "One cch message as a trace step. Bodies are never stored, so the step names
  the peer, id, and channel number only."
  ([route delivery] (message-step route delivery {}))
  ([route {:keys [message_id source target status created_at seq]} names]
  (let [outgoing? (= source route)
        peer (if outgoing? target source)]
    {:at-ms created_at
     :kind :message
     :summary (str (if outgoing? "→ " "← ")
                   (peer-label peer names)
                   " · " message_id
                   (when seq (str " · seq " seq)))
     :outcome (case status
                ("delivered" "submitted" "queued" "duplicate") :ok
                "failed" :failed
                :pending)
     :detail peer})))

(defn place-messages
  "Put each message step into the turn that was running when it was sent: the
  latest turn starting at or before it. Messages before the first turn go to a
  leading turn. Steps within each turn are re-sorted by time. Pure."
  [turns message-steps]
  (let [turns (mapv (fn [t]
                      (-> t
                          (assoc :start-ms (event-ts->ms (:started-at t)))
                          (update :steps (fn [steps]
                                           (mapv #(assoc % :at-ms (event-ts->ms (:at %)))
                                                 steps)))))
                    turns)
        starts (mapv :start-ms turns)
        index-for (fn [ms]
                    (let [i (dec (count (take-while #(and % (<= % ms)) starts)))]
                      (when (<= 0 i) i)))
        {placed true early false}
        (group-by #(boolean (and (:at-ms %) (index-for (:at-ms %)))) message-steps)
        turns (reduce (fn [ts step]
                        (update-in ts [(index-for (:at-ms step)) :steps] conj step))
                      turns placed)
        turns (mapv #(update % :steps (fn [steps] (vec (sort-by (fnil :at-ms 0) steps))))
                    turns)]
    (if (seq early)
      (let [early (vec (sort-by (fnil :at-ms 0) early))]
        (into [{:turn-key "messages-before-first-turn"
                :start-ms (:at-ms (first early))
                :prompts []
                :steps early}]
              turns))
      turns)))

;; --- Rendering ---

(def ^:private kind-label
  {:tool "tool" :permission "permission" :hook-decision "hook" :compaction "compact"
   :subagent "subagent" :notification "notice" :cwd "cwd" :message "message"})

(def ^:private outcome-class
  {:ok "dot-allow" :failed "dot-deny" :denied "dot-deny" :asked "dot-ask"
   :pending "dot-ask" :unknown "dot-observe"})

(def ^:private long-text 600)

(defn- text-block [class text]
  (when-not (str/blank? text)
    (if (<= (count text) long-text)
      [:div {:class class} text]
      [:details {:class class}
       [:summary (subs text 0 long-text) "…"]
       [:div.trace-full text]])))

(defn- step-row [zone {:keys [kind summary outcome duration-ms at-ms tool]}]
  [:li.trace-step {:class (str "trace-kind-" (name (or kind :tool)))}
   [:span.trace-time.mono.xs.muted (local-time at-ms zone clock)]
   [:span {:class (str "dot " (outcome-class outcome "dot-observe"))
           :title (some-> outcome name)}]
   [:span.trace-kind.xs.upper.muted (kind-label kind (some-> kind name))]
   [:span.trace-summary.mono {:title (or tool summary)} summary]
   [:span.trace-duration.xs.muted (duration-label duration-ms)]])

(defn- turn-section [zone now-ms {:keys [start-ms ended-at prompts steps closing-message]}]
  (let [end-ms (event-ts->ms ended-at)
        running? (and (seq prompts) (nil? ended-at))]
    [:section.trace-turn
     [:header.trace-turn-head
      [:span.mono.sm (local-time start-ms zone day-clock)]
      (cond
        running? [:span.trace-badge.trace-running "running"]
        (and start-ms end-ms) [:span.xs.muted (duration-label (- end-ms start-ms))])
      [:span.xs.muted (str (count steps) " step" (when (not= 1 (count steps)) "s"))]]
     (for [{:keys [text]} prompts]
       (text-block "trace-prompt" text))
     (when (seq steps)
       [:ol.trace-steps (map #(step-row zone %) steps)])
     (text-block "trace-reply" closing-message)
     (when (and running? now-ms start-ms)
       [:div.xs.muted (str "started " (duration-label (- now-ms start-ms)) " ago")])]))

(defn page-body
  "Hiccup for one session's trace, newest turn first."
  [{:keys [agent session-id cwd turns messages zone now-ms names]}]
  (let [route (route-id agent session-id)
        turns (place-messages turns (map #(message-step route % names) messages))]
    (list
      [:div.page-header
       [:h1 (or (name-cell agent session-id names)
                (session-label agent session-id))]
       [:div.page-actions
        [:a.btn {:href (str "/trace?session=" session-id)} "↻ refresh"]
        [:a.btn {:href (str "/events?session=" session-id)} "raw events"]]]
      (when-let [status (session-status agent session-id names)]
        [:p.trace-status status])
      [:p.page-subtitle.mono.sm
       (session-label agent session-id) " · "
       (if (= "codex" agent) "Codex" "Claude") " · " route
       (when cwd (str " · " cwd))]
      (if (empty? turns)
        [:p.muted "No recorded activity for this session."]
        (map #(turn-section zone now-ms %) (reverse turns))))))

(defn index-body
  "Hiccup listing recent sessions, each linking to its trace."
  [{:keys [sessions zone names]}]
  (list
    [:div.page-header [:h1 "Trace"]]
    [:p.page-subtitle "What each agent session on this machine has been doing, turn by turn."]
    (if (empty? sessions)
      [:p.muted "No sessions recorded yet."]
      [:table.dense-table
       [:thead [:tr [:th "name"] [:th "id"] [:th "agent"] [:th "where"] [:th "last activity"]]]
       [:tbody
        (for [{:keys [session_id agent cwd timestamp]} sessions
              :when (#{"claude-code" "codex"} agent)]
          [:tr
           [:td [:a {:href (str "/trace?session=" session_id)}
                 (or (name-cell agent session_id names) "—")]
            (when-let [status (session-status agent session_id names)]
              [:div.xs.muted.trace-status-line status])]
           [:td.mono.xs [:a {:href (str "/trace?session=" session_id)}
                         (session-label agent session_id)]]
           [:td.sm (if (= "codex" agent) "Codex" "Claude")]
           [:td.mono.xs.muted {:title cwd} (some-> cwd (str/replace #"^.*/" ""))]
           [:td.mono.xs.muted (local-time (event-ts->ms timestamp) zone day-clock)]])]])))
