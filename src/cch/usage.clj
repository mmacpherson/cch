(ns cch.usage
  "Server-rendered 7-day rate-limit window page.

  Renders the current usage trajectory as an SVG chart: observed
  used_percentage over time, plus an EWMA-derived projection from now to
  the window reset, with a band spanning the fast/slow EWMA estimates.

  Pure functions of the data bundle from cch.ewma/current-window — easy
  to test without a server."
  (:require [cch.ewma :as ewma])
  (:import (java.time Instant ZoneId)
           (java.time.format DateTimeFormatter)))

;; --- chart geometry ---

(def ^:private chart-w 880)
(def ^:private chart-h 280)
(def ^:private margin {:top 24 :right 32 :bottom 36 :left 56})

(defn- plot-area
  "Inner rect for the data — viewBox is [0 0 chart-w chart-h]."
  []
  {:x0 (:left margin)
   :y0 (:top margin)
   :x1 (- chart-w (:right margin))
   :y1 (- chart-h (:bottom margin))})

(defn- y-max
  "Pick a y-axis ceiling that always includes 100 and the projection band's
  top, rounded up to a tidy 10s. Capped so a runaway projection doesn't
  squash the rest of the chart."
  [data]
  (let [hi (max 100.0
                (or (:proj-hi data) 0.0)
                (apply max 0.0 (map :pct (:observed data))))]
    (-> hi (/ 10.0) Math/ceil long (* 10) (min 200))))

(defn- scale-x [{:keys [window-start resets-at]} {:keys [x0 x1]}]
  (let [span (- resets-at window-start)]
    (fn [t] (+ x0 (* (- t window-start) (/ (- x1 x0) span))))))

(defn- scale-y [y-top {:keys [y0 y1]}]
  (fn [pct]
    (- y1 (* pct (/ (- y1 y0) y-top)))))

;; --- formatting helpers ---

(def ^:private day-fmt
  (.withZone (DateTimeFormatter/ofPattern "MMM d") (ZoneId/systemDefault)))

(defn- fmt-day [epoch]
  (.format day-fmt (Instant/ofEpochSecond epoch)))

(defn- points-attr
  "SVG polyline points string from [[x y] ...]."
  [pts]
  (->> pts (map (fn [[x y]] (str x "," y))) (interpose " ") (apply str)))

(defn- band-path
  "Closed path for the band region: along upper from now→reset, then back
   along lower. Used for SVG <path d=...>."
  [pts-upper pts-lower]
  (let [start (first pts-upper)
        body  (concat
                [(str "M " (first start) " " (second start))]
                (for [[x y] (rest pts-upper)] (str "L " x " " y))
                (for [[x y] (reverse pts-lower)] (str "L " x " " y))
                ["Z"])]
    (apply str (interpose " " body))))

;; --- chart svg ---

(defn chart-svg
  "Render the usage chart as Hiccup. Pure function of the data bundle.
   Returns a [:svg ...] tree, or a [:p ...] fallback when there's no data."
  [data]
  (if (or (nil? data) (empty? (:observed data)))
    [:p.has-text-grey
     "Not enough rate-limit data yet to plot. The page populates as the "
     "statusLine reports usage."]
    (let [{:keys [observed resets-at window-start now last-pct
                  proj-pct proj-lo proj-hi]} data
          rect (plot-area)
          y-top (y-max data)
          sx   (scale-x data rect)
          sy   (scale-y y-top rect)
          {:keys [x0 y0 x1 y1]} rect
          ;; observed polyline points
          obs-pts (mapv (fn [{:keys [ts pct]}] [(sx ts) (sy pct)]) observed)
          ;; projection lines from "now" → resets_at
          proj-x0 (sx now)
          proj-x1 (sx resets-at)
          proj-mid [[proj-x0 (sy last-pct)] [proj-x1 (sy proj-pct)]]
          proj-up  [[proj-x0 (sy last-pct)] [proj-x1 (sy proj-hi)]]
          proj-lo' [[proj-x0 (sy last-pct)] [proj-x1 (sy proj-lo)]]
          ;; gridlines: every 25 percent (0,25,50,75,100,...) up to y-top
          y-ticks (range 0 (inc y-top) 25)
          ;; date ticks: each 24h boundary inside the window
          day-ticks (->> (iterate #(+ % 86400) window-start)
                         (take-while #(<= % resets-at)))]
      [:svg {:viewBox (str "0 0 " chart-w " " chart-h)
             :width   "100%"
             :role    "img"
             :class   "usage-chart"}
       ;; --- gridlines + y-axis labels ---
       (for [pct y-ticks
             :let [y (sy pct)]]
         [:g
          [:line {:x1 x0 :x2 x1 :y1 y :y2 y
                  :stroke "var(--bulma-border)"
                  :stroke-width 1
                  :stroke-dasharray (when-not (= pct 100) "2 4")
                  :class (when (= pct 100) "ref-100")}]
          [:text {:x (- x0 8) :y (+ y 4)
                  :text-anchor "end"
                  :font-size 10
                  :fill "var(--bulma-text-weak)"}
           (str pct "%")]])
       ;; --- date ticks on x-axis ---
       (for [t day-ticks
             :let [x (sx t)]]
         [:g
          [:line {:x1 x :x2 x :y1 y1 :y2 (+ y1 4)
                  :stroke "var(--bulma-border)" :stroke-width 1}]
          [:text {:x x :y (+ y1 18)
                  :text-anchor "middle"
                  :font-size 10
                  :fill "var(--bulma-text-weak)"}
           (fmt-day t)]])
       ;; --- "now" vertical guide ---
       (let [x (sx now)]
         [:g
          [:line {:x1 x :x2 x :y1 y0 :y2 y1
                  :stroke "var(--bulma-text-weak)"
                  :stroke-width 1
                  :stroke-dasharray "3 3"}]
          [:text {:x (+ x 4) :y (+ y0 12)
                  :font-size 10
                  :fill "var(--bulma-text-weak)"}
           "now"]])
       ;; --- projection band ---
       [:path {:d (band-path proj-up proj-lo')
               :fill "rgba(124, 58, 237, 0.15)"
               :stroke "none"}]
       ;; --- projection center line ---
       [:polyline {:points (points-attr proj-mid)
                   :fill "none"
                   :stroke "#7c3aed"
                   :stroke-width 2
                   :stroke-dasharray "5 4"}]
       ;; --- observed line ---
       (when (seq obs-pts)
         [:polyline {:points (points-attr obs-pts)
                     :fill "none"
                     :stroke "#059669"
                     :stroke-width 2}])
       ;; --- observed points ---
       (for [[x y] obs-pts]
         [:circle {:cx x :cy y :r 2.5 :fill "#059669"}])])))

(defn legend
  "Tiny inline legend explaining the lines."
  []
  [:div.legend
   [:span.swatch.observed]    " observed "
   [:span.swatch.projection] " projection (slow EWMA) "
   [:span.swatch.band]       " fast/slow band "
   [:span.swatch.ref100]     " 100% reference"])

(defn summary-stats
  "Right-rail readout that mirrors what the statusLine shows."
  [{:keys [last-pct slow_x fast_x proj-pct proj-lo proj-hi
           resets-at now samples]}]
  (let [hours-left (max 0.0 (/ (- resets-at now) 3600.0))]
    [:div.usage-stats
     [:div.stat [:div.k "current"]    [:div.v (format "%.0f%%" last-pct)]]
     [:div.stat [:div.k "pace ×"]     [:div.v (format "%.2f" slow_x)
                                       [:span.aux (format " (fast %.2f)" fast_x)]]]
     [:div.stat [:div.k "projected"]  [:div.v (format "%.0f%%" proj-pct)
                                       [:span.aux (format " (band %.0f–%.0f%%)"
                                                          proj-lo proj-hi)]]]
     [:div.stat [:div.k "resets in"]  [:div.v (format "%.1f h" hours-left)]]
     [:div.stat [:div.k "samples"]    [:div.v (str samples)]]]))

(def page-css
  "div.usage-grid { display: grid; grid-template-columns: 1fr 220px; gap: 1.5em; align-items: start; }
   div.usage-chart-wrap { background: var(--bulma-scheme-main); border: 1px solid var(--bulma-border); border-radius: 6px; padding: 0.75em; }
   svg.usage-chart .ref-100 { stroke: #dc2626; stroke-dasharray: none; }
   div.usage-stats { display: grid; grid-template-columns: 1fr; gap: 0.6em; font-family: var(--bulma-family-primary); }
   div.usage-stats .stat { padding: 0.5em 0.75em; background: var(--bulma-scheme-main); border: 1px solid var(--bulma-border); border-radius: 4px; }
   div.usage-stats .k { font-size: 0.7em; text-transform: uppercase; letter-spacing: 0.05em; color: var(--bulma-text-weak); }
   div.usage-stats .v { font-family: var(--bulma-family-code); font-size: 1.05em; }
   div.usage-stats .v .aux { font-size: 0.75em; color: var(--bulma-text-weak); margin-left: 0.4em; }
   div.legend { font-size: 0.8em; color: var(--bulma-text-weak); margin-top: 0.5em; }
   div.legend .swatch { display: inline-block; width: 1.2em; height: 0.5em; vertical-align: middle; margin: 0 0.2em 0 0.6em; border-radius: 1px; }
   div.legend .swatch.observed   { background: #059669; }
   div.legend .swatch.projection { background: repeating-linear-gradient(to right, #7c3aed 0 5px, transparent 5px 9px); }
   div.legend .swatch.band       { background: rgba(124, 58, 237, 0.25); }
   div.legend .swatch.ref100     { background: #dc2626; }")

(defn page-body
  "Hiccup body for the /usage page. Caller wraps with html/head/nav."
  [data]
  [:div.usage-grid
   [:div
    [:div.usage-chart-wrap (chart-svg data)]
    (legend)]
   (when data (summary-stats data))])

(defn build-data
  "Public entry point — fetches the bundle from ewma. Indirection kept
  so tests can pass synthetic bundles to chart-svg directly."
  []
  (ewma/current-window))
