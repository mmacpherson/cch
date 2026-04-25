(ns cch.usage
  "Server-rendered 7-day rate-limit window page.

  Renders observed used_percentage as an SVG chart, plus one forward
  projection line per method from cch.projections. The Bayesian
  credible interval is shown as a band; other methods are shown as
  lines only (their bands are tabulated in the right-hand stats panel).

  Pure functions of the data bundle from cch.ewma/current-window — easy
  to test without a server."
  (:require [cch.ewma :as ewma])
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

(defn- y-max
  "Pick a y-axis ceiling that always includes 100 and the highest
  projection (with band), rounded up to a tidy 10s. Capped so a
  runaway projection doesn't squash the chart."
  [data]
  (let [proj-max (apply max 0.0
                        (mapcat (fn [{:keys [proj band]}]
                                  [proj (or (:hi band) proj)])
                                (:projections data)))
        hi (max 100.0
                proj-max
                (apply max 0.0 (map :pct (:observed data))))]
    (-> hi (/ 10.0) Math/ceil long (* 10) (min 200))))

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

;; --- projection method styling ---

(def ^:private method-style
  "Color + display order per projection method. Methods not in this
   map are still rendered (with a default gray) but lose stable
   ordering across page loads."
  {:ewma         {:color "#7c3aed" :order 0}
   :ols          {:color "#2563eb" :order 1}
   :bayes        {:color "#f59e0b" :order 2}
   :trailing-6h  {:color "#94a3b8" :order 3}
   :trailing-24h {:color "#475569" :order 4}})

(defn- method-color [m]
  (get-in method-style [m :color] "#6b7280"))

(defn- ordered-projections [projections]
  (sort-by #(get-in method-style [(:method %) :order] 99) projections))

;; --- chart svg ---

(defn chart-svg
  "Render the usage chart. Pure function of the data bundle. Returns a
  [:svg ...] tree, or a [:p ...] fallback when there's no data."
  [data]
  (if (or (nil? data) (empty? (:observed data)))
    [:p.has-text-grey
     "Not enough rate-limit data yet to plot. The page populates as the "
     "statusLine reports usage."]
    (let [{:keys [observed resets-at window-start now last-pct projections]} data
          rect (plot-area)
          y-top (y-max data)
          sx   (scale-x data rect)
          sy   (scale-y y-top rect)
          {:keys [x0 y0 x1 y1]} rect
          obs-pts (mapv (fn [{:keys [ts pct]}] [(sx ts) (sy pct)]) observed)
          proj-x0 (sx now)
          proj-x1 (sx resets-at)
          y-ticks (range 0 (inc y-top) 25)
          day-ticks (->> (iterate #(+ % 86400) window-start)
                         (take-while #(<= % resets-at)))
          line-for (fn [proj-pct] [[proj-x0 (sy last-pct)] [proj-x1 (sy proj-pct)]])
          ;; rgba helper so each band fills with its method's color at low alpha
          rgba (fn [hex a]
                 (let [r (Integer/parseInt (subs hex 1 3) 16)
                       g (Integer/parseInt (subs hex 3 5) 16)
                       b (Integer/parseInt (subs hex 5 7) 16)]
                   (format "rgba(%d,%d,%d,%.2f)" r g b a)))]
      [:svg {:viewBox (str "0 0 " chart-w " " chart-h)
             :width   "100%"
             :role    "img"
             :class   "usage-chart"}
       ;; clipPath so projection bands/lines that exceed the y-axis
       ;; ceiling (a runaway EWMA at 500% etc.) get cropped to the
       ;; plot rectangle instead of bleeding into the header.
       [:defs
        [:clipPath {:id "plot-clip"}
         [:rect {:x x0 :y y0 :width (- x1 x0) :height (- y1 y0)}]]]
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
                  :text-anchor "end" :font-size 10
                  :fill "var(--bulma-text-weak)"}
           (str pct "%")]])
       ;; --- date ticks on x-axis ---
       (for [t day-ticks
             :let [x (sx t)]]
         [:g
          [:line {:x1 x :x2 x :y1 y1 :y2 (+ y1 4)
                  :stroke "var(--bulma-border)" :stroke-width 1}]
          [:text {:x x :y (+ y1 18)
                  :text-anchor "middle" :font-size 10
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
       ;; --- per-method bands and projection lines, clipped to plot area ---
       [:g {:clip-path "url(#plot-clip)"}
        ;; bands first so lines render on top
        (for [{:keys [method band]} (ordered-projections projections)
              :when band]
          [:path {:d (band-path (line-for (:hi band))
                                (line-for (:lo band)))
                  :fill (rgba (method-color method) 0.18)
                  :stroke "none"
                  :class "band-region"
                  :data-method (name method)}])
        (for [{:keys [method proj]} (ordered-projections projections)]
          [:polyline {:points (points-attr (line-for proj))
                      :fill "none"
                      :stroke (method-color method)
                      :stroke-width 2.5
                      :stroke-dasharray "5 4"
                      :class (str "proj-line proj-" (name method))
                      :data-method (name method)}])]
       ;; --- observed line + points ---
       (when (seq obs-pts)
         [:polyline {:points (points-attr obs-pts)
                     :fill "none"
                     :stroke "#059669"
                     :stroke-width 2}])
       (for [[x y] obs-pts]
         [:circle {:cx x :cy y :r 2.5 :fill "#059669"}])])))

(defn legend
  "Inline legend. Hovering a method entry isolates that method on the
   chart (others fade) via the CSS rules in page-css."
  [data]
  [:div.legend
   [:span.legend-item
    [:span.swatch.observed] " observed"]
   (for [{:keys [method name]} (ordered-projections (:projections data))]
     [:span.legend-item {:data-method (clojure.core/name method)}
      [:span.swatch {:style (str "background:" (method-color method))}]
      " " name])
   [:span.legend-item
    [:span.swatch.ref100] " 100% reference"]])

(defn- fmt-band [{:keys [lo hi]}]
  (when (and lo hi (not= lo hi))
    (format " (band %.0f–%.0f%%)" (double lo) (double hi))))

(defn summary-stats
  "Right-rail readout. Lists each method's projected end-of-window
   percent, with confidence band when the method provides one."
  [{:keys [last-pct projections resets-at now samples]}]
  (let [hours-left (max 0.0 (/ (- resets-at now) 3600.0))]
    [:div.usage-stats
     [:div.stat [:div.k "current"]   [:div.v (format "%.0f%%" (double last-pct))]]
     [:div.stat [:div.k "resets in"] [:div.v (format "%.1f h" hours-left)]]
     [:div.stat [:div.k "samples"]   [:div.v (str samples)]]
     [:div.method-projections
      [:div.k "projected at reset"]
      (for [{:keys [method name proj band]} (ordered-projections projections)]
        [:div.method-row
         [:span.swatch {:style (str "background:" (method-color method))}]
         [:span.method-name name]
         [:span.method-proj (format "%.0f%%" (double proj))
          (when-let [b (fmt-band band)] [:span.aux b])]])]]))

(def page-css
  "div.usage-grid { display: grid; grid-template-columns: 1fr 280px; gap: 1.5em; align-items: start; }
   div.usage-chart-block { background: var(--bulma-scheme-main); border: 1px solid var(--bulma-border); border-radius: 6px; padding: 0.75em; }
   svg.usage-chart .ref-100 { stroke: #dc2626; stroke-dasharray: none; }
   svg.usage-chart .proj-line { transition: opacity 0.18s ease; }
   svg.usage-chart .band-region { transition: opacity 0.18s ease; }
   div.legend { font-size: 0.8em; color: var(--bulma-text-weak); margin-top: 0.6em; display: flex; flex-wrap: wrap; gap: 0.6em 1.1em; line-height: 1.6; }
   div.legend .legend-item { display: inline-flex; align-items: center; gap: 0.35em; padding: 0.05em 0.25em; border-radius: 3px; cursor: default; transition: background 0.18s ease; }
   div.legend .legend-item[data-method]:hover { background: var(--bulma-scheme-main-bis); color: var(--bulma-text); }
   div.legend .swatch { display: inline-block; width: 1.2em; height: 0.5em; vertical-align: middle; border-radius: 1px; }
   div.legend .swatch.observed { background: #059669; }
   div.legend .swatch.ref100   { background: #dc2626; }
   /* Hover-isolate: when any legend method item is hovered, dim everything;
      then per-method rules below restore opacity for the matching ones. */
   div.usage-chart-block:has(div.legend .legend-item[data-method]:hover) svg.usage-chart .proj-line { opacity: 0.12; }
   div.usage-chart-block:has(div.legend .legend-item[data-method]:hover) svg.usage-chart .band-region { opacity: 0.04; }
   div.usage-chart-block:has(div.legend .legend-item[data-method=\"ewma\"]:hover) svg.usage-chart [data-method=\"ewma\"]:not(.legend-item) { opacity: 1; }
   div.usage-chart-block:has(div.legend .legend-item[data-method=\"ols\"]:hover) svg.usage-chart [data-method=\"ols\"]:not(.legend-item) { opacity: 1; }
   div.usage-chart-block:has(div.legend .legend-item[data-method=\"bayes\"]:hover) svg.usage-chart [data-method=\"bayes\"]:not(.legend-item) { opacity: 1; }
   div.usage-chart-block:has(div.legend .legend-item[data-method=\"trailing-6h\"]:hover) svg.usage-chart [data-method=\"trailing-6h\"]:not(.legend-item) { opacity: 1; }
   div.usage-chart-block:has(div.legend .legend-item[data-method=\"trailing-24h\"]:hover) svg.usage-chart [data-method=\"trailing-24h\"]:not(.legend-item) { opacity: 1; }
   /* For the band specifically, when its method is isolated, lift its opacity higher than the dimmed default. */
   div.usage-chart-block:has(div.legend .legend-item[data-method=\"ewma\"]:hover) svg.usage-chart .band-region[data-method=\"ewma\"],
   div.usage-chart-block:has(div.legend .legend-item[data-method=\"ols\"]:hover) svg.usage-chart .band-region[data-method=\"ols\"],
   div.usage-chart-block:has(div.legend .legend-item[data-method=\"bayes\"]:hover) svg.usage-chart .band-region[data-method=\"bayes\"] { opacity: 1; }
   /* Side panel — usage-stats and per-method projections list */
   div.usage-stats { display: flex; flex-direction: column; gap: 0.6em; font-family: var(--bulma-family-primary); }
   div.usage-stats .stat { padding: 0.5em 0.75em; background: var(--bulma-scheme-main); border: 1px solid var(--bulma-border); border-radius: 4px; }
   div.usage-stats .k { font-size: 0.7em; text-transform: uppercase; letter-spacing: 0.05em; color: var(--bulma-text-weak); }
   div.usage-stats .v { font-family: var(--bulma-family-code); font-size: 1.05em; }
   div.usage-stats .v .aux { font-size: 0.75em; color: var(--bulma-text-weak); margin-left: 0.4em; }
   div.method-projections { padding: 0.5em 0.75em; background: var(--bulma-scheme-main); border: 1px solid var(--bulma-border); border-radius: 4px; display: flex; flex-direction: column; gap: 0.35em; }
   div.method-row { display: grid; grid-template-columns: 0.9em 1fr auto; gap: 0.4em; align-items: center; font-family: var(--bulma-family-code); font-size: 0.85em; }
   div.method-row .swatch { width: 0.9em; height: 0.5em; border-radius: 1px; }
   div.method-row .method-name { font-family: var(--bulma-family-primary); font-size: 0.8em; color: var(--bulma-text-weak); }
   div.method-row .method-proj .aux { color: var(--bulma-text-weak); font-size: 0.75em; }")

(defn page-body
  "Hiccup body for the /usage page. Caller wraps with html/head/nav."
  [data]
  [:div.usage-grid
   ;; Single block wraps chart + legend so :has() can reach from a
   ;; legend hover into the SVG to isolate the matching method.
   [:div.usage-chart-block
    (chart-svg data)
    (legend data)]
   (when data (summary-stats data))])

(defn build-data
  "Public entry point — fetches the bundle from ewma. Indirection kept
   so tests can pass synthetic bundles to chart-svg directly."
  []
  (ewma/current-window))
