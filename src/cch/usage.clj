(ns cch.usage
  "Server-rendered 7-day rate-limit window page.

  Renders observed used_percentage as an SVG chart, plus a single
  Bayesian forward projection (mean line + 90% credible interval band)
  from cch.projections.

  Pure functions of the data bundle from cch.forecast/current-window —
  easy to test without a server."
  (:require [cch.forecast :as forecast]
            [cch.projections :as proj])
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
  (.withZone (DateTimeFormatter/ofPattern "MMM d") (ZoneId/systemDefault)))

(defn- fmt-day [epoch]
  (.format day-fmt (Instant/ofEpochSecond epoch)))

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


;; --- chart svg ---

(defn chart-svg
  "Render the usage chart. Pure function of the data bundle. Returns a
  [:svg ...] tree, or a [:p ...] fallback when there's no data."
  [data]
  (if (nil? data)
    [:p {:style "color: var(--fg-muted);"}
     "Not enough rate-limit data yet to plot. The page populates as the "
     "statusLine reports usage."]
    (let [{:keys [observed resets-at window-start now last-pct projection]} data
          rect (plot-area)
          y-top (y-max data)
          sx   (scale-x data rect)
          sy   (scale-y y-top rect)
          {:keys [x0 y0 x1 y1]} rect
          obs-pts    (mapv (fn [{:keys [ts pct]}] [(sx ts) (sy pct)]) observed)
          smoothed   (when (seq observed)
                       (some->> (proj/loess-smooth observed 80 0.08)
                                (mapv (fn [{:keys [ts pct]}] [(sx ts) (sy pct)]))))
          proj-x0 (sx now)
          proj-x1 (sx resets-at)
          y-ticks (range 0 (inc y-top) 25)
          day-ticks (->> (iterate #(+ % 86400) window-start)
                         (take-while #(<= % resets-at)))
          line-for (fn [proj-pct] [[proj-x0 (sy last-pct)] [proj-x1 (sy proj-pct)]])
          rgba (fn [hex a]
                 (let [r (Integer/parseInt (subs hex 1 3) 16)
                       g (Integer/parseInt (subs hex 3 5) 16)
                       b (Integer/parseInt (subs hex 5 7) 16)]
                   (format "rgba(%d,%d,%d,%.2f)" r g b a)))]
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
       ;; --- reset-cycle vertical lines (resets-at, resets-at-24h, ...) ---
       ;; Marks the same hour-of-day as the weekly reset, every 24h back to
       ;; window-start. Subtle — drawn before gridlines so they sit furthest back.
       (for [t (->> (iterate #(- % 86400) resets-at)
                    (take-while #(>= % window-start)))
             :let [x (sx t)]]
         [:line {:x1 x :x2 x :y1 (sy 100) :y2 y1
                 :stroke "var(--fg-muted)"
                 :stroke-width 1
                 :opacity 0.25
                 :class "reset-cycle-tick"}])
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
       ;; --- date ticks on x-axis ---
       (for [t day-ticks
             :let [x (sx t)]]
         [:g
          [:line {:x1 x :x2 x :y1 y1 :y2 (+ y1 4)
                  :stroke "var(--border)" :stroke-width 1}]
          [:text {:x x :y (+ y1 18)
                  :text-anchor "middle" :font-size 10
                  :fill "var(--fg-muted)"}
           (fmt-day t)]])
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
       ;; --- projection band + line, clipped to plot area ---
       (when projection
         (let [{:keys [proj band]} projection]
           [:g {:clip-path "url(#plot-clip)"}
            (when band
              [:path {:d (band-path (line-for (:hi band))
                                    (line-for (:lo band)))
                      :fill (rgba projection-color 0.10)
                      :stroke "none"
                      :class "band-region"}])
            [:polyline {:points (points-attr (line-for proj))
                        :fill "none"
                        :stroke projection-color
                        :stroke-width 2.7
                        :stroke-opacity 0.9
                        :stroke-dasharray "5 4"
                        :class "proj-line"}]]))
       ;; --- observed: smoothed line + small raw dots ---
       (when (seq smoothed)
         [:polyline {:points (points-attr smoothed)
                     :fill "none"
                     :stroke "#059669"
                     :stroke-width 2.1
                     :stroke-opacity 0.9
                     :class "observed-smoothed"}])
       (for [[x y] obs-pts]
         [:circle {:cx x :cy y :r 1.6
                   :fill "#059669"
                   :fill-opacity 0.5
                   :class "observed-point"}])])))

(defn legend
  "Inline legend: observed series + projected usage swatches."
  [_data]
  [:div.legend
   [:span.legend-item
    [:span.swatch.observed] " observed"]
   [:span.legend-item
    [:span.swatch {:style (str "background:" projection-color)}]
    " projected (90% CI)"]])


;; page-css removed — all styles live in cch.css now

(def ^:private rate-chart-h 280)

(defn rate-chart-svg
  "Second chart: burn rate (%/hr) sampled on a regular time grid.
   For each grid point t, finds the oldest and newest observed sample
   within a trailing lookback window and computes (delta-pct / delta-t).
   Same math as the statusline burn indicator, applied historically."
  [data]
  (when (and data (> (count (:observed data)) 4))
    (let [{:keys [rate-5h-samples window-start resets-at now]} data
          ;; Use 5h-window samples (60s buckets, ~33 windows in the 7d span).
          ;; Ticks ~7.5x more often than the 7d window, giving a continuous
          ;; signal even when the 7d curve is on a long plateau.
          ;; Scale factor 0.133 converts 5h-%/hr to 7d-%/hr units (empirical
          ;; from observed co-movement: 1% of 5h window ~ 0.133% of 7d window).
          scale       0.133
          in-5h       (vec (filter #(<= (:ts %) now) (or rate-5h-samples [])))
          n-5h        (count in-5h)
          grid-step-s (* 10 60)
          lookback-s  (* 30 60)
          grid-ts     (range window-start (+ now 1) grid-step-s)
          ;; Binary search: first index where (:ts v[i]) >= cutoff. O(log n).
          lower-bound (fn [cutoff]
                        (loop [lo 0 hi n-5h]
                          (if (>= lo hi) lo
                            (let [mid (quot (+ lo hi) 2)]
                              (if (< (:ts (nth in-5h mid)) cutoff)
                                (recur (inc mid) hi)
                                (recur lo mid))))))
          rate-pts    (mapv (fn [t]
                              (let [lo      (lower-bound (- t lookback-s))
                                    hi      (lower-bound (inc t))
                                    bucket  (subvec in-5h lo hi)
                                    active-r (when (seq bucket)
                                               (apply max (map :resets-at bucket)))
                                    active  (filter #(= (:resets-at %) active-r) bucket)
                                    rate    (when (>= (count active) 2)
                                              (let [oldest  (first active)
                                                    newest  (last active)
                                                    elapsed (- (:ts newest) (:ts oldest))]
                                                (when (>= elapsed 300)
                                                  (* scale
                                                     (max 0.0 (/ (- (:pct newest) (:pct oldest))
                                                                  (/ elapsed 3600.0)))))))]
                                {:ts t :rate (or rate 0.0)}))
                            grid-ts)
          margin-r  {:top 34 :right 32 :bottom 36 :left 56}
          rect      {:x0 (:left margin-r) :y0 (:top margin-r)
                     :x1 (- chart-w (:right margin-r))
                     :y1 (- rate-chart-h (:bottom margin-r))}
          {:keys [x0 y0 x1 y1]} rect
          ;; y ceiling: p95 of *non-zero* rates so that the many idle-period
          ;; zeros don't pull the percentile below the active-session peaks.
          ;; Hard cap at 25 to clip genuine millisecond bursts (e.g. 4
          ;; sessions all ticking simultaneously in a few seconds).
          ;; y ceiling: max of non-zero rates, hard-capped at 25 to clip
          ;; sub-second multi-session coincident bursts (e.g. 140%/hr).
          nonzero   (filter pos? (map :rate rate-pts))
          r-max     (min 25.0 (max 2.0 (if (seq nonzero) (apply max nonzero) 2.0)))
          y-top     (-> r-max (/ 5.0) Math/ceil long (* 5) (max 5))
          sx        (scale-x {:window-start window-start :resets-at resets-at} rect)
          sy        (scale-y y-top rect)
          r-ticks   (range 0 (inc y-top) 5)
          pts       (mapv (fn [{:keys [ts rate]}] [(sx ts) (sy rate)]) rate-pts)
          ;; Area fill: close path at y1 (zero line)
          area-d    (when (seq pts)
                      (let [[fx fy] (first pts)
                            [lx _]  (last pts)]
                        (str "M " fx " " fy " "
                             (apply str (for [[x y] (rest pts)] (str "L " x " " y " ")))
                             "L " lx " " y1 " L " fx " " y1 " Z")))]
      [:svg {:viewBox (str "0 0 " chart-w " " rate-chart-h)
             :width   "100%"
             :class   "rate-chart"}
       [:text {:x (/ (+ x0 x1) 2.0) :y 14
               :text-anchor "middle" :font-size 10
               :fill "var(--fg-muted)"}
        "burn rate (%/hr)"]
       [:defs
        [:clipPath {:id "rate-clip"}
         [:rect {:x x0 :y y0 :width (- x1 x0) :height (- y1 y0)}]]]
       ;; gridlines + y-axis labels
       (for [r r-ticks :let [y (sy r)]]
         [:g
          [:line {:x1 x0 :x2 x1 :y1 y :y2 y
                  :stroke "var(--border)" :stroke-width 1
                  :stroke-dasharray (when (pos? r) "2 4")}]
          [:text {:x (- x0 8) :y (+ y 4) :text-anchor "end" :font-size 10
                  :fill "var(--fg-muted)"}
           (str r "%/h")]])
       ;; x-axis date ticks (aligned with main chart)
       (for [t (->> (iterate #(+ % 86400) window-start) (take-while #(<= % resets-at)))
             :let [x (sx t)]]
         [:g
          [:line {:x1 x :x2 x :y1 y1 :y2 (+ y1 4)
                  :stroke "var(--border)" :stroke-width 1}]
          [:text {:x x :y (+ y1 16) :text-anchor "middle" :font-size 10
                  :fill "var(--fg-muted)"}
           (fmt-day t)]])
       ;; "now" guide
       (let [x (sx now)]
         [:line {:x1 x :x2 x :y1 y0 :y2 y1
                 :stroke "var(--fg-muted)" :stroke-width 1
                 :stroke-dasharray "3 3"}])
       ;; reset-cycle vertical ticks (same as main chart)
       (for [t (->> (iterate #(- % 86400) resets-at) (take-while #(>= % window-start)))
             :let [x (sx t)]]
         [:line {:x1 x :x2 x :y1 (sy (min y-top 100)) :y2 y1
                 :stroke "var(--fg-muted)" :stroke-width 1 :opacity 0.25}])
       ;; area fill + line, clipped
       [:g {:clip-path "url(#rate-clip)"}
        (when area-d
          [:path {:d area-d :fill "#059669" :fill-opacity 0.12 :stroke "none"}])
        (when (seq pts)
          [:polyline {:points (points-attr pts)
                      :fill "none" :stroke "#059669"
                      :stroke-width 1.75 :stroke-opacity 0.9
                      :class "rate-line"}])]])))

(defn page-body
  "Hiccup body for the /usage page. Caller wraps with html/head/nav."
  [data]
  [:div.usage-chart-block
   (chart-svg data)
   (legend data)
   (rate-chart-svg data)])

(defn build-data
  "Public entry point — fetches the bundle from forecast. Indirection
   kept so tests can pass synthetic bundles to chart-svg directly."
  []
  (forecast/current-window))
