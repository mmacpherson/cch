(ns cch.overview
  "Overview landing page — stat tiles, activity strip, top hooks, recent events."
  (:require [cch.forecast :as forecast]
            [cch.log :as log]
            [cch.stats :as stats]))

;; --- Stat tiles ---

(defn- format-pct [n]
  (when n (str (Math/round (double n)) "%")))

(defn- format-secs-left [s]
  (when (and s (pos? s))
    (let [h (quot s 3600)
          m (quot (mod s 3600) 60)]
      (cond
        (>= h 24) (str (quot h 24) "d " (mod h 24) "h")
        (pos? h)  (str h "h " m "m")
        :else     (str m "m")))))

(defn- stat-tile [label value & {:keys [subtitle css-class]}]
  [:div.stat-tile {:class css-class}
   [:div.stat-label label]
   [:div.stat-value value]
   (when subtitle [:div.stat-sub subtitle])])

(defn- quota-tiles
  "Four stat tiles from forecast data."
  [fc]
  (let [{:keys [current_pct projected_pct secs_left local_rate_phr band]} (:seven_day fc)]
    [:div.tile-row
     (stat-tile "used" (or (format-pct current_pct) "—")
                :subtitle "7-day window")
     (stat-tile "projected"
                (list (or (format-pct projected_pct) "—")
                      (when band
                        [:span {:style "font-size: 0.55em; opacity: 0.7; margin-left: 0.4em; font-weight: 400"}
                         (str (:lo band) "–" (:hi band) "%")]))
                :subtitle "at reset"
                :css-class (when (and projected_pct (> projected_pct 85)) "warn"))
     (stat-tile "resets in" (or (format-secs-left secs_left) "—")
                :subtitle "next window")
     (stat-tile "burn" (if local_rate_phr
                         (str (format "%.1f" (double local_rate_phr)) "%/h")
                         "—")
                :subtitle "current rate")]))

;; --- Activity strip (24h hourly bars) ---

(defn- activity-strip-svg
  "Inline SVG: one bar per hour, last 24 hours."
  [hourly-data]
  (let [max-n   (max 1 (apply max 1 (map :n hourly-data)))
        bar-w   28
        gap     3
        total-w (* 24 (+ bar-w gap))
        h       80
        bars    (for [hr (range 24)
                      :let [label (format "%02d" hr)
                            n     (or (some #(when (= (:hour %) label) (:n %)) hourly-data) 0)
                            bar-h (max 2 (* (/ n max-n) (- h 16)))
                            x     (* hr (+ bar-w gap))
                            y     (- h bar-h 14)]]
                  (list
                    [:rect {:x x :y y :width bar-w :height bar-h
                            :rx 2 :fill "var(--accent)" :opacity (if (zero? n) 0.15 0.7)}]
                    [:text {:x (+ x (/ bar-w 2)) :y (- h 2)
                            :text-anchor "middle" :fill "var(--fg-muted)"
                            :font-size 9 :font-family "var(--family-mono)"}
                     label]))]
    [:svg {:xmlns "http://www.w3.org/2000/svg"
           :viewBox (str "0 0 " total-w " " h)
           :style "width:100%; height:auto; max-height:100px;"}
     bars]))

;; --- Top hooks ---

(defn- hook-row [{:keys [hook_name fires denies]}]
  [:tr
   [:td [:span.hook-badge {:style (str "border-color: var(--c-"
                                       (case hook_name
                                         "command-audit" "ask"
                                         "command-guard" "deny"
                                         "scope-lock"    "ask"
                                         "protect-files" "deny"
                                         "push-gate"     "allow"
                                         "observe")
                                       ")")}
         hook_name]]
   [:td.mono fires]
   [:td.mono {:style "color: var(--c-deny)"} (when (and denies (pos? denies)) denies)]])

;; --- Recent events mini-table ---

(defn- mini-event-row [{:keys [timestamp hook_name event_type tool_name decision]}]
  (let [short-ts (when timestamp
                   (subs timestamp (min 11 (count timestamp))
                         (min 19 (count timestamp))))]
    [:tr {:class (when (and (= hook_name "event-log") (nil? decision)) "observed")}
     [:td.mono {:style "color: var(--fg-muted)"} short-ts]
     [:td [:span.hook-badge {:style (str "border-color: var(--c-"
                                         (case hook_name
                                           "command-audit" "ask"
                                           "command-guard" "deny"
                                           "scope-lock"    "ask"
                                           "protect-files" "deny"
                                           "push-gate"     "allow"
                                           "observe")
                                         ")")}
           hook_name]]
     [:td event_type]
     [:td.mono {:style "color: var(--fg-muted)"} (or tool_name "—")]]))

;; --- Page assembly ---

(defn build-data
  "Assemble all data for the overview page."
  []
  {:forecast  (forecast/statusline-stats)
   :events-today (stats/event-count-today)
   :denies-today (stats/deny-count-today)
   :latency      (stats/avg-dispatch-latency)
   :hourly       (stats/hourly-activity-24h)
   :top-hooks    (stats/top-hooks)
   :recent       (log/query-events :limit 8)})

(defn page-body
  "Hiccup body for the overview page."
  [{:keys [forecast events-today denies-today latency hourly top-hooks recent]}]
  [:div.page-wrap
   [:div.page-header
    [:div
     [:h1 "overview"]
     [:p.subtitle "at-a-glance system health and activity"]]]

   ;; Stat tiles
   (quota-tiles forecast)

   [:div.tile-row
    (stat-tile "events today" (or events-today 0))
    (stat-tile "denies today" (or denies-today 0)
               :css-class (when (and denies-today (pos? denies-today)) "warn"))
    (stat-tile "avg latency" (if-let [ms (:avg_ms latency)]
                               (let [v (double ms)]
                                 (if (< v 1) "<1ms" (str (Math/round v) "ms")))
                               "—")
               :subtitle (when-let [n (:n latency)] (str n " timed")))
    (stat-tile "hooks active" (count top-hooks))]

   ;; Activity strip
   [:div.surface {:style "margin-top: 1.5rem; padding: 1rem 1.2rem;"}
    [:h3 {:style "margin-bottom: 0.5rem; font-size: var(--font-sm); color: var(--fg-muted); text-transform: uppercase; letter-spacing: 0.05em;"} "activity · last 24h"]
    (activity-strip-svg hourly)]

   ;; Two-column: top hooks + recent events
   [:div.overview-grid
    ;; Top hooks
    [:div.surface
     [:h3 {:style "margin-bottom: 0.5rem; font-size: var(--font-sm); color: var(--fg-muted); text-transform: uppercase; letter-spacing: 0.05em;"} "top hooks · 7 days"]
     [:table.dense-table
      [:thead [:tr [:th "hook"] [:th "fires"] [:th "denies"]]]
      [:tbody
       (for [h top-hooks] (hook-row h))]]]

    ;; Recent events
    [:div.surface
     [:h3 {:style "margin-bottom: 0.5rem; font-size: var(--font-sm); color: var(--fg-muted); text-transform: uppercase; letter-spacing: 0.05em;"} "recent activity"]
     [:table.dense-table
      [:thead [:tr [:th "time"] [:th "hook"] [:th "event"] [:th "tool"]]]
      [:tbody
       (for [e recent] (mini-event-row e))]]
     ]]])
