(ns cch.web
  "Shared cch page chrome for runner-local and fleet-scoped web surfaces."
  (:require [clojure.java.io :as io]))

(def nav-mark
  "The cch navigation mark. It uses currentColor so every surface shares the
  same visual identity without loading a separate image."
  [:svg {:xmlns "http://www.w3.org/2000/svg"
         :width 22 :height 22 :viewBox "0 0 24 24"
         :fill "none" :stroke "currentColor" :stroke-width 2
         :stroke-linecap "round" :stroke-linejoin "round"
         :aria-hidden "true"}
   [:path {:d "m17.586 11.414-5.93 5.93a1 1 0 0 1-8-8l3.137-3.137a.707.707 0 0 1 1.207.5V10"}]
   [:path {:d "M20.414 8.586 22 7"}]
   [:circle {:cx 19 :cy 10 :r 2}]])

(def control-nav-mark
  "Hosted control-plane variant of the hook mark. The outlined white badge
  distinguishes the fleet/meta surface while preserving the runner identity."
  [:svg {:xmlns "http://www.w3.org/2000/svg"
         :width 24 :height 24 :viewBox "0 0 24 24"
         :aria-hidden "true"}
   [:rect {:x 1 :y 1 :width 22 :height 22 :rx 6
           :fill "white" :stroke "currentColor" :stroke-width 1.5}]
   [:g {:transform "translate(3 3) scale(.75)"
        :fill "none" :stroke "currentColor" :stroke-width 2.4
        :stroke-linecap "round" :stroke-linejoin "round"}
    [:path {:d "m17.586 11.414-5.93 5.93a1 1 0 0 1-8-8l3.137-3.137a.707.707 0 0 1 1.207.5V10"}]
    [:path {:d "M20.414 8.586 22 7"}]
    [:circle {:cx 19 :cy 10 :r 2}]]])

(def control-favicon-svg
  "Standalone SVG for the authenticated hosted favicon route."
  (str "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
       "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"32\" height=\"32\" "
       "viewBox=\"0 0 32 32\">"
       "<rect x=\"1.5\" y=\"1.5\" width=\"29\" height=\"29\" rx=\"8\" "
       "fill=\"white\" stroke=\"#059669\" stroke-width=\"2\"/>"
       "<g transform=\"translate(4 4)\" fill=\"none\" stroke=\"#059669\" "
       "stroke-width=\"2.4\" stroke-linecap=\"round\" stroke-linejoin=\"round\">"
       "<path d=\"m17.586 11.414-5.93 5.93a1 1 0 0 1-8-8l3.137-3.137a.707.707 0 0 1 1.207.5V10\"/>"
       "<path d=\"M20.414 8.586 22 7\"/>"
       "<circle cx=\"19\" cy=\"10\" r=\"2\"/>"
       "</g></svg>"))

(def ^:private asset-version (str (System/currentTimeMillis)))

(defn page-head
  "Runner-local page head. Hosted pages inline `base-css` because their
  listener deliberately exposes no unauthenticated static-file surface."
  [{:keys [title]}]
  [:head
   [:meta {:charset "utf-8"}]
   [:title (str "cch · " title)]
   [:meta {:name "viewport" :content "width=device-width,initial-scale=1"}]
   [:link {:rel "icon" :type "image/svg+xml" :href "/favicon.svg"}]
   [:link {:rel "preconnect" :href "https://fonts.googleapis.com"}]
   [:link {:rel "preconnect" :href "https://fonts.gstatic.com" :crossorigin true}]
   [:link {:rel "stylesheet"
           :href "https://fonts.googleapis.com/css2?family=JetBrains+Mono:wght@400;500&display=block"}]
   [:link {:rel "stylesheet" :href (str "/cch.css?v=" asset-version)}]
   [:script {:type "module"
             :src "https://cdn.jsdelivr.net/gh/starfederation/datastar@v1.0.0-RC.8/bundles/datastar.js"}]])

(def ^:private base-css-content
  (delay (slurp (io/resource "public/cch.css"))))

(defn base-css
  "The exact stylesheet used by runner-local pages, suitable for embedding in
  the authenticated hosted document."
  []
  @base-css-content)

(defn nav-bar
  "Shared navigation. `tabs` is a sequence of [key label href]; `status` and
  `actions` occupy the quiet right-hand status area."
  [{:keys [active tabs status actions mark]}]
  (let [tab (fn [[key label href]]
              (if (= active key)
                [:span.nav-tab.active {:aria-current "page"} label]
                [:a.nav-tab {:href href} label]))]
    [:nav.nav-wrap
     [:a.nav-brand {:href "/"}
      [:span.nav-icon (or mark nav-mark)]
      "cch"]
     [:div.nav-tabs (map tab tabs)]
     [:div.nav-status
      (when status [:span.dot-online])
      (when status [:span status])
      actions]]))
