(ns cch.usage
  "Server-rendered rate-limit window page (5h or 7d).

  Renders observed used_percentage as a step line and the usage model's
  predictive fan to the reset (median path, 50% and 90% bands), plus a
  per-hour chart of used and expected usage. Before the model has enough
  history, the projection falls back to a straight line and band.

  Pure functions of the data bundle from cch.forecast/current-window —
  easy to test without a server."
  (:require [cch.forecast :as forecast]
            [clojure.string :as str])
  (:import (java.time Instant ZoneId)
           (java.time.format DateTimeFormatter)))

;; --- chart geometry ---

(def ^:private chart-w 880)
(def ^:private chart-h 280)
(def ^:private margin {:top 24 :right 32 :bottom 36 :left 56})

(defn- plot-area []
  {:x0 (:left margin)
   :y0 (:top margin)
   :x1 (- chart-w (:right margin))
   :y1 (- chart-h (:bottom margin))})

(defn- y-max [_data] 125)

(defn- scale-x [{:keys [window-start resets-at]} {:keys [x0 x1]}]
  ;; Ratios sneak in when integer dividends collide with integer divisors.
  ;; Force doubles end-to-end — browsers reject "184951/525" in SVG point
  ;; lists and silently drop the polyline.
  (let [span (double (- resets-at window-start))
        slope (/ (double (- x1 x0)) span)]
    (fn [t] (+ (double x0) (* (- (double t) window-start) slope)))))

(defn- scale-y [y-top {:keys [y0 y1]}]
  (let [slope (/ (double (- y1 y0)) (double y-top))]
    (fn [pct] (- (double y1) (* (double pct) slope)))))

;; --- formatting helpers ---

(def ^:private day-fmt
  (.withZone (DateTimeFormatter/ofPattern "EEE MMM d") (ZoneId/systemDefault)))

(def ^:private hour-fmt
  (.withZone (DateTimeFormatter/ofPattern "HH:mm") (ZoneId/systemDefault)))

(defn- fmt-day [epoch]
  (.format day-fmt (Instant/ofEpochSecond epoch)))

(defn- fmt-hour [epoch]
  (.format hour-fmt (Instant/ofEpochSecond epoch)))

(defn- local-midnights
  "Local midnights strictly inside (from, to]."
  [from to]
  (let [zone (ZoneId/systemDefault)
        first-day (-> (Instant/ofEpochSecond from) (.atZone zone) .toLocalDate (.plusDays 1))]
    (->> (iterate #(.plusDays ^java.time.LocalDate % 1) first-day)
         (map #(.toEpochSecond (.atStartOfDay ^java.time.LocalDate % zone)))
         (take-while #(<= % to)))))

(defn- x-axis-ticks
  "Time-axis ticks for the given window. Returns {:ts seq, :fmt fn, :step}.
   7d: local midnights, labeled by weekday and date. 5h: one tick per hour."
  [{:keys [window-key window-start resets-at]}]
  (if (= window-key :five-hour)
    {:ts   (->> (iterate #(+ % 3600) window-start)
                (take-while #(<= % resets-at)))
     :fmt  fmt-hour
     :step 3600}
    {:ts   (local-midnights window-start resets-at)
     :fmt  fmt-day
     :step 86400}))

(defn- points-attr [pts]
  (->> pts (map (fn [[x y]] (str x "," y))) (interpose " ") (apply str)))

(defn- band-path
  "Closed path for the band region: along upper from now→reset, then
   back along lower."
  [pts-upper pts-lower]
  (let [start (first pts-upper)
        body  (concat
                [(str "M " (first start) " " (second start))]
                (for [[x y] (rest pts-upper)] (str "L " x " " y))
                (for [[x y] (reverse pts-lower)] (str "L " x " " y))
                ["Z"])]
    (apply str (interpose " " body))))

;; --- projection styling ---

(def ^:private projection-color "#f59e0b") ; orange
(def ^:private observed-color "#059669")   ; green

(defn- rgba [hex a]
  (let [r (Integer/parseInt (subs hex 1 3) 16)
        g (Integer/parseInt (subs hex 3 5) 16)
        b (Integer/parseInt (subs hex 5 7) 16)]
    (format "rgba(%d,%d,%d,%.2f)" r g b (double a))))

(def ^:private cap-fmt
  {:seven-day (.withZone (DateTimeFormatter/ofPattern "EEE HH:mm") (ZoneId/systemDefault))
   :five-hour hour-fmt})

(defn- step-points
  "Observed usage as a step line. The meter moves in whole-percent steps,
  so usage holds at its last report until the next one, then jumps."
  [observed sx sy now]
  (let [pts (reduce (fn [acc {:keys [ts pct]}]
                      (let [x (sx ts) y (sy pct)]
                        (if-let [[_ prev-y] (peek acc)]
                          (conj acc [x prev-y] [x y])
                          (conj acc [x y]))))
                    [] observed)]
    (if-let [[_ y] (peek pts)]
      (conj pts [(sx now) y])
      pts)))

(defn- cap-crossing
  "First path point whose median demand reaches the cap, or nil."
  [path]
  (first (filter #(>= (:median %) 100.0) path)))

(defn- fan
  "Predictive fan from `now` to the reset: 90% and 50% bands and the median
  path. They follow the activity profile, so they flatten when you are
  usually idle."
  [{:keys [path]} last-pct now sx sy]
  (let [start {:ts now :lo last-pct :q25 last-pct :median last-pct
               :q75 last-pct :hi last-pct}
        pts (cons start path)
        line (fn [k] (mapv (fn [p] [(sx (:ts p)) (sy (k p))]) pts))]
    [:g {:clip-path "url(#plot-clip)"}
     [:path {:d (band-path (line :hi) (line :lo))
             :fill (rgba projection-color 0.12) :stroke "none"
             :class "band-region"}]
     [:path {:d (band-path (line :q75) (line :q25))
             :fill (rgba projection-color 0.24) :stroke "none"
             :class "band-inner"}]
     [:polyline {:points (points-attr (line :median))
                 :fill "none" :stroke projection-color
                 :stroke-width 2.4 :stroke-opacity 0.95
                 :stroke-dasharray "5 4"
                 :class "proj-line"}]]))

(defn- straight-projection
  "Fallback before the model has enough history: a straight line from now
  to the projected value, with its band."
  [{:keys [proj band]} last-pct now resets-at sx sy]
  (let [line-for (fn [pct] [[(sx now) (sy last-pct)] [(sx resets-at) (sy pct)]])]
    [:g {:clip-path "url(#plot-clip)"}
     (when band
       [:path {:d (band-path (line-for (:hi band)) (line-for (:lo band)))
               :fill (rgba projection-color 0.10) :stroke "none"
               :class "band-region"}])
     [:polyline {:points (points-attr (line-for proj))
                 :fill "none" :stroke projection-color
                 :stroke-width 2.7 :stroke-opacity 0.9
                 :stroke-dasharray "5 4"
                 :class "proj-line"}]]))

;; --- chart svg ---

(defn chart-svg
  "Render the usage chart. Pure function of the data bundle. Returns a
  [:svg ...] tree, or a [:p ...] fallback when there's no data."
  [data]
  (if (nil? data)
    [:p {:style "color: var(--fg-muted);"}
     "Not enough rate-limit data yet to plot. The page populates as the "
     "statusLine reports usage."]
    (let [{:keys [observed resets-at window-start now last-pct projection window-key]} data
          rect (plot-area)
          y-top (y-max data)
          sx   (scale-x data rect)
          sy   (scale-y y-top rect)
          {:keys [x0 y0 x1 y1]} rect
          y-ticks (range 0 (inc y-top) 25)
          x-ticks (x-axis-ticks data)
          crossing (cap-crossing (:path projection))]
      [:svg {:viewBox (str "0 0 " chart-w " " chart-h)
             :width   "100%"
             :role    "img"
             :class   "usage-chart"}
       [:text {:x (/ (+ x0 x1) 2.0) :y 14
               :text-anchor "middle" :font-size 10
               :fill "var(--fg-muted)"}
        "quota used (%)"]
       [:defs
        [:clipPath {:id "plot-clip"}
         [:rect {:x x0 :y y0 :width (- x1 x0) :height (- y1 y0)}]]]
       ;; --- day separators at local midnight (7d only) ---
       ;; Drawn before gridlines so they sit furthest back. Days, not reset
       ;; hours, because the forecast follows the daily activity rhythm.
       (when-not (= window-key :five-hour)
         (for [t (:ts x-ticks)
               :let [x (sx t)]]
           [:line {:x1 x :x2 x :y1 (sy 100) :y2 y1
                   :stroke "var(--fg-muted)"
                   :stroke-width 1
                   :opacity 0.25
                   :class "day-tick"}]))
       ;; --- linear-pace reference line (0% at window-start → 100% at resets-at) ---
       ;; If observed usage is above this line you're ahead of pace; below means behind.
       [:line {:x1 (sx window-start) :x2 (sx resets-at)
               :y1 (sy 0) :y2 (sy 100)
               :stroke "var(--fg-muted)"
               :stroke-width 1
               :stroke-dasharray "6 3"
               :opacity 0.55
               :class "ref-pace"}]
       ;; --- gridlines + y-axis labels ---
       (for [pct y-ticks
             :let [y (sy pct)]]
         [:g
          [:line {:x1 x0 :x2 x1 :y1 y :y2 y
                  :stroke "var(--border)"
                  :stroke-width 1
                  :stroke-dasharray (when-not (= pct 100) "2 4")
                  :class (when (= pct 100) "ref-100")}]
          [:text {:x (- x0 8) :y (+ y 4)
                  :text-anchor "end" :font-size 10
                  :fill "var(--fg-muted)"}
           (str pct "%")]])
       ;; --- date/hour ticks on x-axis ---
       (for [t (:ts x-ticks)
             :let [x (sx t)]]
         [:g
          [:line {:x1 x :x2 x :y1 y1 :y2 (+ y1 4)
                  :stroke "var(--border)" :stroke-width 1}]
          [:text {:x x :y (+ y1 18)
                  :text-anchor "middle" :font-size 10
                  :fill "var(--fg-muted)"}
           ((:fmt x-ticks) t)]])
       ;; --- "now" vertical guide ---
       (let [x (sx now)]
         [:g
          [:line {:x1 x :x2 x :y1 y0 :y2 y1
                  :stroke "var(--fg-muted)"
                  :stroke-width 1
                  :stroke-dasharray "3 3"}]
          [:text {:x (+ x 4) :y (+ y0 12)
                  :font-size 10
                  :fill "var(--fg-muted)"}
           "now"]])
       ;; --- forecast: fan from the model, or the straight fallback ---
       (when projection
         (if (seq (:path projection))
           (fan projection last-pct now sx sy)
           (straight-projection projection last-pct now resets-at sx sy)))
       ;; --- where the median path reaches the cap ---
       (when crossing
         (let [x (sx (:ts crossing)) y (sy 100)
               label (str "median hits cap "
                          (.format ^DateTimeFormatter (cap-fmt (or window-key :seven-day))
                                   (Instant/ofEpochSecond (:ts crossing))))]
           [:g {:class "cap-marker"}
            [:circle {:cx x :cy y :r 3.5 :fill "var(--c-deny)"}]
            [:text {:x (if (> x (- x1 150)) (- x 6) (+ x 6)) :y (- y 6)
                    :text-anchor (if (> x (- x1 150)) "end" "start")
                    :font-size 10 :fill "var(--c-deny)"}
             label]]))
       ;; --- observed usage ---
       (when (seq observed)
         [:polyline {:points (points-attr (step-points observed sx sy now))
                     :fill "none"
                     :stroke observed-color
                     :stroke-width 2.1
                     :stroke-opacity 0.9
                     :class "observed-line"}])])))

(defn legend
  "Inline legend for the usage chart."
  [data]
  (let [fan? (seq (get-in data [:projection :path]))]
    [:div.legend
     [:span.legend-item
      [:span.swatch.observed] " observed"]
     [:span.legend-item
      [:span.swatch {:style (str "background:" projection-color)}]
      (if fan? " median forecast" " projected (90% CI)")]
     (when fan?
       [:span.legend-item
        [:span.swatch {:style (str "background:" (rgba projection-color 0.45))}]
        " 50% likely"])
     (when fan?
       [:span.legend-item
        [:span.swatch {:style (str "background:" (rgba projection-color 0.2))}]
        " 90% likely"])]))

;; page-css removed — all styles live in cch.css now

(def ^:private bars-chart-h 200)

(defn- usage-buckets
  "Per-bucket usage: observed increments before now, and the model's
  expected (mean) increments after it. The bucket holding `now` carries
  both, stacked. Returns [{:t :observed :expected}]."
  [{:keys [window-start resets-at now observed projection]} step]
  (let [obs (vec observed)
        cum-at (fn [t] (reduce (fn [v {:keys [ts pct]}] (if (<= ts t) pct (reduced v)))
                               0.0 obs))
        bucket (fn [t] (* step (quot (- t window-start) step)))
        observed-by (into {}
                          (for [b (range 0 (- now window-start) step)
                                :let [t (+ window-start b)]]
                            [b (max 0.0 (- (cum-at (min now (+ t step))) (cum-at t)))]))
        expected-by (second
                      (reduce (fn [[prev m] {:keys [ts mean]}]
                                [mean (update m (bucket (dec ts)) (fnil + 0.0) (- mean prev))])
                              [(double (or (:last-pct projection) (cum-at now))) {}]
                              (:path projection)))]
    (for [b (range 0 (- resets-at window-start) step)]
      {:t (+ window-start b)
       :observed (get observed-by b 0.0)
       :expected (get expected-by b 0.0)})))

(defn usage-bars-svg
  "Second chart: usage per bucket (hour for 7d, 5 min for 5h). Solid bars
  are what was used; light bars are what the model expects, which is the
  activity profile scaled by the current intensity."
  [data]
  (when (and data (seq (:observed data)))
    (let [{:keys [window-key window-start resets-at now]} data
          five? (= window-key :five-hour)
          step (if five? 300 3600)
          rows (usage-buckets (assoc-in data [:projection :last-pct] (:last-pct data)) step)
          margin-r {:top 30 :right 32 :bottom 30 :left 56}
          {:keys [x0 y0 x1 y1]} {:x0 (:left margin-r) :y0 (:top margin-r)
                                 :x1 (- chart-w (:right margin-r))
                                 :y1 (- bars-chart-h (:bottom margin-r))}
          tallest (reduce max 1.0 (map #(+ (:observed %) (:expected %)) rows))
          y-top (-> tallest Math/ceil long (max 2))
          sx (scale-x {:window-start window-start :resets-at resets-at} {:x0 x0 :x1 x1})
          sy (scale-y y-top {:y0 y0 :y1 y1})
          bar-w (max 0.5 (- (sx (+ window-start step)) (sx window-start) 0.6))
          x-ticks (x-axis-ticks data)]
      [:svg {:viewBox (str "0 0 " chart-w " " bars-chart-h)
             :width "100%"
             :class "rate-chart usage-bars"}
       [:text {:x (/ (+ x0 x1) 2.0) :y 14
               :text-anchor "middle" :font-size 10
               :fill "var(--fg-muted)"}
        (str "usage per " (if five? "5 min" "hour") " (%) — solid: used, light: expected")]
       (for [v [0 y-top] :let [y (sy v)]]
         [:g
          [:line {:x1 x0 :x2 x1 :y1 y :y2 y
                  :stroke "var(--border)" :stroke-width 1
                  :stroke-dasharray (when (pos? v) "2 4")}]
          [:text {:x (- x0 8) :y (+ y 4) :text-anchor "end" :font-size 10
                  :fill "var(--fg-muted)"}
           (str v "%")]])
       (for [t (:ts x-ticks) :let [x (sx t)]]
         [:g
          [:line {:x1 x :x2 x :y1 y1 :y2 (+ y1 4) :stroke "var(--border)" :stroke-width 1}]
          [:text {:x x :y (+ y1 16) :text-anchor "middle" :font-size 10
                  :fill "var(--fg-muted)"}
           ((:fmt x-ticks) t)]])
       (let [x (sx now)]
         [:line {:x1 x :x2 x :y1 y0 :y2 y1
                 :stroke "var(--fg-muted)" :stroke-width 1 :stroke-dasharray "3 3"}])
       (for [{:keys [t observed expected]} rows
             :let [x (+ (sx t) 0.3)]]
         [:g
          (when (pos? observed)
            [:rect {:x x :y (sy observed) :width bar-w :height (- y1 (sy observed))
                    :fill observed-color :fill-opacity 0.75 :class "bar-observed"}])
          (when (pos? expected)
            [:rect {:x x :y (sy (+ observed expected)) :width bar-w
                    :height (- (sy observed) (sy (+ observed expected)))
                    :fill projection-color :fill-opacity 0.35 :class "bar-expected"}])])])))

(defn page-body
  "Hiccup body for the /usage page. Caller wraps with html/head/nav."
  [data]
  [:div.usage-chart-block
   (chart-svg data)
   (legend data)
   (usage-bars-svg data)])

(defn usage-href
  "Build a Usage link while preserving agent and window selection. `base` is
  normally /usage for both local and hosted applications."
  [base {:keys [window agent]}]
  (let [params (cond-> []
                 (= window :five-hour) (conj "window=5h")
                 (and agent (not= agent "claude-code"))
                 (conj (str "agent=" agent)))]
    (if (seq params)
      (str base "?" (str/join "&" params))
      base)))

(defn filter-strip
  "Shared window and agent controls for local and fleet-scoped Usage pages."
  [base active-window active-agent]
  [:div.filter-strip
   [:div.filter-group
    [:span.filter-label "Window"]
    [:div.filter-tabs
     (for [[key label] [[:five-hour "5h"] [:seven-day "7d"]]]
       [:a.filter-tab {:href (usage-href base {:window key
                                               :agent active-agent})
                       :class (when (= key active-window) "active")
                       :aria-current (when (= key active-window) "page")}
        label])]]
   [:div.filter-group
    [:span.filter-label "Source"]
    [:div.filter-tabs
     (for [[agent label] [["claude-code" "Claude"]
                          ["codex" "Codex"]
                          ["agy" "AGY"]]]
       [:a.filter-tab {:href (usage-href base {:window active-window
                                               :agent agent})
                       :class (when (= agent active-agent) "active")
                       :aria-current (when (= agent active-agent) "page")}
        label])]]])

(defn- duration-label [seconds]
  (when (and seconds (pos? seconds))
    (let [hours (quot seconds 3600)
          minutes (quot (mod seconds 3600) 60)]
      (cond
        (>= hours 24) (str (quot hours 24) "d " (mod hours 24) "h")
        (pos? hours) (str hours "h " minutes "m")
        :else (str minutes "m")))))

(defn stat-tiles
  "Shared stat row derived solely from a rich Usage data bundle."
  [data]
  (let [{:keys [last-pct resets-at now rate-phr projection]} data
        projected (:proj projection)
        p-cap (:p-cap projection)
        band (:band projection)
        seconds-left (when (and resets-at now) (max 0 (- resets-at now)))]
    [:div.tile-row
     [:div.stat-tile
      [:div.stat-label "used"]
      [:div.stat-value (if (number? last-pct)
                         (str (Math/round (double last-pct)) "%") "—")]]
     [:div.stat-tile {:class (when (and projected (> projected 85)) "warn")}
      [:div.stat-label "projected at reset"]
      [:div.stat-value (if (number? projected)
                         (str (Math/round (double projected)) "%") "—")
       (when band
         [:span {:style "font-size:.55em;opacity:.7;margin-left:.4em;font-weight:400"}
          (str (Math/round (double (:lo band))) "–"
               (Math/round (double (:hi band))) "%")])]]
     [:div.stat-tile
      [:div.stat-label "resets in"]
      [:div.stat-value (or (duration-label seconds-left) "—")]]
     [:div.stat-tile
      [:div.stat-label "burn rate"]
      [:div.stat-value (if (number? rate-phr)
                         (format "%.1f%%/h" (double rate-phr)) "—")]]
     [:div.stat-tile {:class (when (and p-cap (>= p-cap 0.25)) "warn")}
      [:div.stat-label "chance of cap"]
      [:div.stat-value (if (number? p-cap)
                         (str (Math/round (* 100.0 (double p-cap))) "%") "—")]]]))

(defn page-view
  "The full Usage experience shared by local and hosted data adapters."
  [data {:keys [base window agent] :or {base "/usage"}}]
  [:div
   (filter-strip base window agent)
   (stat-tiles data)
   (page-body data)])

(defn build-data
  "Public entry point — fetches the bundle from forecast. Indirection
   kept so tests can pass synthetic bundles to chart-svg directly.
   `agent` defaults to forecast/default-agent ('claude-code');
   `window-key` defaults to :seven-day."
  ([] (forecast/current-window))
  ([window-key] (forecast/current-window window-key))
  ([agent window-key] (forecast/current-window agent window-key)))
