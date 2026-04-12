(ns cch.server
  "Long-lived HTTP dispatcher for cch hooks.

  Runs on localhost and exposes three kinds of endpoints:

    POST /hooks/<name>   → dispatch to the named hook's composed handler
    GET  /               → server-rendered dashboard HTML (events table + filters)
    GET  /health         → liveness + registered hooks (JSON)

  Dispatch goes to the same `composed` handler that command-mode uses
  (defined by defhook in cch.core). Logging/timing/error semantics are
  identical across command and HTTP modes.

  Dashboard is server-rendered via hiccup, styled with Pico.css +
  Google Fonts (Roboto / Roboto Condensed). No client JS — filter
  changes are plain GET form submits, auto-refresh via
  <meta http-equiv=\"refresh\">."
  (:require [cch.log :as log]
            [cch.protocol :as proto]
            [cheshire.core :as json]
            [cli.registry :as registry]
            [clojure.java.shell :as shell]
            [clojure.string :as str]
            [hiccup2.core :as hic]
            [org.httpkit.server :as httpkit]))

;; --- Hook registration ---

(defn- load-hook
  "Require a hook's namespace and return {:name :composed :description}.
  Returns nil if the namespace can't be loaded."
  [hook-name {:keys [ns description]}]
  (try
    (require (symbol ns))
    (let [composed (ns-resolve (symbol ns) 'composed)]
      (when composed
        {:name     hook-name
         :ns       ns
         :description description
         :composed @composed}))
    (catch Exception e
      (binding [*out* *err*]
        (println (format "cch.server: failed to load hook '%s' (%s): %s"
                         hook-name ns (.getMessage e))))
      nil)))

(defn- build-registry
  "Load every hook in cli.registry that resolves. Returns a name→entry map."
  []
  (into {} (keep (fn [[n h]] (some-> (load-hook n h) (as-> e [n e])))
                 (registry/list-hooks))))

;; --- Dashboard (server-rendered HTML via hiccup) ---

(defn- worktree-root-of
  "Infer a worktree root from a cwd. Best-effort — if the path isn't a git
  repo or shell-out fails, returns the cwd itself."
  [cwd]
  (try
    (let [result (shell/sh "git" "-C" (or cwd ".") "rev-parse" "--show-toplevel")]
      (if (zero? (:exit result))
        (str/trim (:out result))
        cwd))
    (catch Exception _ cwd)))

(defn- repo-label
  "Short human-readable label for a worktree root path. Usually the
  basename; for paths whose parent directory name contains \"worktrees\",
  includes the parent too so linked worktrees stay distinguishable from
  the main repo (e.g. dc-worktrees/gondola-ingest)."
  [path]
  (let [segs (remove str/blank? (str/split path #"/"))
        last-two (take-last 2 segs)
        [parent _] last-two]
    (cond
      (empty? segs) path
      (and parent (str/includes? parent "worktrees"))
      (str/join "/" last-two)
      :else
      (last segs))))

(defn- ephemeral-path?
  "Paths under /tmp or /var/folders are test-scratch / temp repos; keep them
  out of the dropdown. Users don't care about repos that no longer exist."
  [path]
  (or (= path "/tmp")
      (str/starts-with? path "/tmp/")
      (str/starts-with? path "/var/folders/")))

(defn- distinct-cwds
  "All distinct cwds from the events table (not limited to recent rows).
  Small cardinality in practice — one entry per project cch has seen."
  []
  (->> (log/query-events :limit 100000)
       (keep :cwd)
       distinct))

(defn- distinct-repos
  "Distinct cwds from events → worktree roots → filtered → [value label] pairs.
  Label is the basename (or last-two-segments for worktree layouts)."
  []
  (let [roots (->> (distinct-cwds)
                   (map worktree-root-of)
                   (remove ephemeral-path?)
                   (into (sorted-set)))]
    (map (fn [r] [r (repo-label r)]) roots)))

(defn- select-with-options
  "Render a <select> from [[value label] ...] pairs, marking `current` selected."
  [name current options]
  [:select {:name name}
   (for [[v label] options]
     [:option (cond-> {:value v}
                (= v current) (assoc :selected "selected"))
      label])])

(def hook-metadata
  "Per-hook display metadata. :color is the badge background; :tooltip is
  the Pico [data-tooltip] text shown on hover so users can decode the
  color code without reading the code."
  {"event-log"     {:color   "#6c757d"
                    :tooltip "Universal observer — logs every event but never blocks"}
   "scope-lock"    {:color   "#d97706"
                    :tooltip "Worktree scope enforcement — prompts for edits outside allowed paths"}
   "command-audit" {:color   "#059669"
                    :tooltip "Bash audit log — records every command; flags configured patterns"}})

(defn- badge
  [hook-name]
  (let [{:keys [color tooltip]} (get hook-metadata hook-name)]
    [:span.badge
     (cond-> {:style (str "background:" (or color "#495057"))}
       tooltip (assoc :data-tooltip tooltip))
     hook-name]))

(defn- pretty-extra
  "Pretty-print extra column JSON; if parse fails, return as-is."
  [extra]
  (try
    (-> (json/parse-string extra true)
        (json/generate-string {:pretty true}))
    (catch Exception _ (or extra ""))))

(defn- dash [v]
  (if (or (nil? v) (and (string? v) (str/blank? v))) "—" v))

(defn- event-card
  "One event rendered as a collapsible <article> — summary row on top,
  full detail on expand. All fields are shown in the detail panel."
  [{:keys [id timestamp session_id hook_name event_type tool_name
           file_path cwd decision reason elapsed_ms extra]}]
  (let [observation? (and (= hook_name "event-log") (nil? decision))
        short-ts     (apply str (take 19 (or timestamp "")))
        short-reason (if reason (apply str (take 80 reason)) "")]
    [:article.event {:class (if observation? "observed" "acted")}
     [:details
      ;; summary is itself the CSS grid. The chevron is a real span (not
      ;; ::before + ::marker) so browser default markers can't double up.
      [:summary
       [:span.chev "▸"]
       [:span.ts short-ts]
       (badge hook_name)
       [:span.evt event_type]
       [:span.tool (dash tool_name)]
       [:span.dec (dash decision)]
       [:span.file (dash file_path)]
       [:span.reason short-reason]]
      [:div.detail
       [:dl
        [:dt "id"]         [:dd id]
        [:dt "timestamp"]  [:dd timestamp]
        [:dt "session"]    [:dd (dash session_id)]
        [:dt "hook"]       [:dd hook_name]
        [:dt "event type"] [:dd event_type]
        [:dt "tool"]       [:dd (dash tool_name)]
        [:dt "cwd"]        [:dd (dash cwd)]
        [:dt "file path"]  [:dd (dash file_path)]
        [:dt "decision"]   [:dd (dash decision)]
        [:dt "reason"]     [:dd (dash reason)]
        [:dt "elapsed ms"] [:dd (if elapsed_ms
                                  (format "%.2f" (double elapsed_ms))
                                  "—")]]
       [:h5 "payload"]
       [:pre (pretty-extra extra)]]]]))

(defn- events-for-hook
  "The events a specific hook registers for, per the registry.
  Handles both single-event (:event) and multi-event (:events) hooks.
  Returns nil for unknown hook names."
  [hook-name]
  (when-let [h (registry/get-hook hook-name)]
    (sort
      (if-let [evs (:events h)]
        (map :event evs)
        (when-let [e (:event h)] [e])))))

(defn- known-events
  "All Claude Code events cch is aware of (via the event-log observer's
  subscription set). Used when no specific hook is selected."
  []
  (events-for-hook "event-log"))

(defn- hook-scoped-events
  "The Event dropdown is narrowed by the currently-selected Hook. When
  no hook is selected, we show the full known-events set."
  [current-hook]
  (if (or (nil? current-hook) (str/blank? current-hook))
    (known-events)
    (or (events-for-hook current-hook) [])))

(defn- distinct-sessions
  "Return up to the 30 most-recently-active session IDs as [[id label]...]
  pairs, optionally scoped to a cwd prefix. Cascades from the Repo
  filter: if a repo is selected, Session only shows that repo's
  sessions. Label is 'YYYY-MM-DDTHH:MM · <uuid-prefix>…'."
  [cwd-prefix]
  (let [events (log/query-events :limit 2000
                                 :cwd-prefix (when-not (str/blank? cwd-prefix)
                                               cwd-prefix))
        uniq   (second
                 (reduce (fn [[seen out] {:keys [session_id timestamp]}]
                           (if (or (nil? session_id) (contains? seen session_id))
                             [seen out]
                             [(conj seen session_id)
                              (conj out [session_id timestamp])]))
                         [#{} []]
                         events))]
    (map (fn [[sid ts]]
           [sid (format "%s · %s…"
                        (apply str (take 16 (or ts "")))
                        (apply str (take 8 sid)))])
         (take 30 uniq))))

(defn- filter-form
  [q repos]
  [:form.filters {:method "get"}
   [:div.grid
    [:label "Repo"
     (select-with-options "cwd-prefix" (:cwd-prefix q "")
       (cons ["" "all"] repos))]
    [:label "Hook"
     (select-with-options "hook" (:hook q "")
       [["" "all"]
        ["event-log" "event-log"]
        ["scope-lock" "scope-lock"]
        ["command-audit" "command-audit"]])]
    [:label "Event"
     (select-with-options "event" (:event q "")
       (cons ["" "all"]
             (for [e (hook-scoped-events (:hook q))] [e e])))]
    [:label "Decision"
     (select-with-options "decision" (:decision q "")
       [["" "all"] ["allow" "allow"] ["ask" "ask"] ["deny" "deny"] ["block" "block"]])]]
   [:div.grid
    [:label "Session"
     (select-with-options "session" (:session q "")
       (cons ["" "all"] (distinct-sessions (:cwd-prefix q))))]
    [:label "Since (SQLite ts)"
     [:input {:type "text" :name "since" :value (:since q "")
              :placeholder "2026-04-12"}]]
    [:label "Limit"
     [:input {:type "number" :name "limit" :value (:limit q "50")
              :min "1" :max "500"}]]
    [:label (hic/raw "&nbsp;")
     [:button {:type "submit"} "Apply"]]]])

(def dashboard-css
  ":root {
     --pico-font-family: 'Roboto', system-ui, sans-serif;
     --pico-font-family-monospace: 'Roboto Mono', monospace;
   }
   h1, h2, h3, h4, h5, th {
     font-family: 'Roboto Condensed', sans-serif;
     letter-spacing: 0.01em;
   }
   h1 { margin-bottom: 0.2em; }
   .subtitle { color: var(--pico-muted-color); margin-top: 0; }
   .filters { margin: 1.5em 0; }
   .filters label { font-size: 0.85em; }
   .meta { color: var(--pico-muted-color); font-size: 0.85em; }

   /* --- Badges --- */
   .badge { display: inline-block; padding: 2px 8px; border-radius: 3px; color: white; font-size: 0.75em; font-weight: 500; letter-spacing: 0.02em; cursor: help; white-space: nowrap; }
   .badge[data-tooltip] { border-bottom: none !important; }

   /* --- Event list --- */
   .event-list article.event { margin: 0; padding: 0; border-radius: 4px; border: none; }
   .event-list article.event + article.event { margin-top: 2px; }
   .event-list details { margin: 0; }

   /* Summary is itself the CSS grid — chevron is the first column, all
      other cells fill the remaining row width. Long values truncate with
      ellipsis instead of wrapping the row. */
   .event-list details summary {
     display: grid;
     grid-template-columns: 1em 11em 7em 10em 6em 5em minmax(10em, 1fr) minmax(8em, 2fr);
     gap: 0.7em;
     align-items: center;
     cursor: pointer;
     padding: 0.35em 0.6em;
     border-radius: 4px;
     font-size: 0.88em;
     list-style: none;
   }
   /* Suppress browser disclosure marker AND Pico's own ::after chevron —
      otherwise both compete with our <span.chev>. */
   .event-list details summary::-webkit-details-marker { display: none; }
   .event-list details summary::marker { display: none; }
   .event-list details summary::after { display: none; }
   .event-list details summary .chev {
     color: var(--pico-muted-color);
     transition: transform 0.1s;
     font-size: 0.9em;
     display: inline-block;
   }
   .event-list details[open] summary .chev { transform: rotate(90deg); }
   .event-list details summary:hover { background: var(--pico-code-background-color); }
   .event-list article.observed summary { opacity: 0.65; }
   .event-list article.acted summary { font-weight: 500; }

   /* Ellipsis on any grid cell whose content can be long. Exclude
      .badge — it hosts the Pico tooltip pseudo-element, which Pico
      positions via bottom:100% relative to the badge; overflow:hidden
      on the badge would clip the tooltip above it. */
   .event-list details summary > *:not(.badge) { overflow: hidden; text-overflow: ellipsis; white-space: nowrap; min-width: 0; }
   .event-list details summary .badge { overflow: visible; }
   .event-list details summary .ts { font-family: 'Roboto Mono', monospace; color: var(--pico-muted-color); font-size: 0.85em; }
   .event-list details summary .evt { font-weight: 500; }
   .event-list details summary .file { font-family: 'Roboto Mono', monospace; font-size: 0.85em; color: var(--pico-muted-color); }
   .event-list details summary .reason { color: var(--pico-muted-color); font-size: 0.85em; font-style: italic; }

   /* Detail panel — shown when <details open> */
   .event-list .detail { padding: 0.8em 1.2em 1em 2em; background: var(--pico-code-background-color); border-radius: 0 0 4px 4px; font-size: 0.9em; }
   .event-list .detail dl { display: grid; grid-template-columns: max-content 1fr; gap: 0.3em 1em; margin: 0 0 1em 0; }
   .event-list .detail dt { font-family: 'Roboto Condensed', sans-serif; color: var(--pico-muted-color); font-size: 0.85em; margin: 0; }
   .event-list .detail dd { margin: 0; font-family: 'Roboto Mono', monospace; font-size: 0.85em; word-break: break-all; }
   .event-list .detail h5 { margin: 1em 0 0.3em 0; font-size: 0.85em; color: var(--pico-muted-color); }
   .event-list .detail pre { font-size: 0.8em; white-space: pre-wrap; word-break: break-word; max-height: 30em; overflow: auto; background: var(--pico-background-color); padding: 0.7em; border-radius: 4px; margin: 0; }")

(defn- dashboard-html
  [q]
  (let [repos  (distinct-repos)
        events (log/query-events
                 :limit       (or (some-> (:limit q) Long/parseLong) 50)
                 :hook        (when-not (str/blank? (:hook q)) (:hook q))
                 :event       (when-not (str/blank? (:event q)) (:event q))
                 :session     (when-not (str/blank? (:session q)) (:session q))
                 :decision    (when-not (str/blank? (:decision q)) (:decision q))
                 :since       (when-not (str/blank? (:since q)) (:since q))
                 :cwd-prefix  (when-not (str/blank? (:cwd-prefix q)) (:cwd-prefix q)))]
    (str "<!doctype html>\n"
         (hic/html
           [:html {:lang "en"}
            [:head
             [:meta {:charset "utf-8"}]
             [:title "cch · events"]
             [:meta {:name "viewport" :content "width=device-width,initial-scale=1"}]
             [:meta {:http-equiv "refresh" :content "30"}]
             [:link {:rel "preconnect" :href "https://fonts.googleapis.com"}]
             [:link {:rel "preconnect" :href "https://fonts.gstatic.com" :crossorigin true}]
             [:link {:rel "stylesheet"
                     :href "https://fonts.googleapis.com/css2?family=Roboto:wght@400;500&family=Roboto+Condensed:wght@500;700&family=Roboto+Mono&display=swap"}]
             [:link {:rel "stylesheet"
                     :href "https://cdn.jsdelivr.net/npm/@picocss/pico@2/css/pico.min.css"}]
             [:style (hic/raw dashboard-css)]]
            [:body
             [:main.container
              [:h1 "cch · events"]
              [:p.subtitle
               "Centralized log of every Claude Code event cch is subscribed to. "
               "Auto-refreshes every 30s."]
              (filter-form q repos)
              [:p.meta
               (format "%d event(s) · click a row to expand · " (count events))
               [:a {:href "/"} "clear filters"]]
              [:div.event-list
               (for [e events] (event-card e))]]]]))))

;; --- Handlers ---

(defn- handle-hook-dispatch
  "POST /hooks/<name> — parse body, run composed handler, serialize response."
  [hooks name req]
  (if-let [{:keys [composed]} (get hooks name)]
    (try
      (let [body-str (slurp (:body req))
            input    (-> (json/parse-string body-str true)
                         (assoc :cch/hook-name name))
            event    (or (:hook_event_name input) "PreToolUse")
            result   (composed input)
            json-out (proto/->response event result)]
        {:status 200
         :headers {"Content-Type" "application/json"}
         :body (or json-out "")})
      (catch Exception e
        (binding [*out* *err*]
          (println (format "cch.server: /hooks/%s error: %s" name (.getMessage e))))
        {:status 500
         :headers {"Content-Type" "application/json"}
         :body (json/generate-string {:error (.getMessage e)})}))
    {:status 404
     :headers {"Content-Type" "application/json"}
     :body (json/generate-string {:error (str "unknown hook: " name)})}))

(defn- parse-query
  "Parse httpkit's :query-string into a keyword-keyed map. Returns {} if nil."
  [qs]
  (if (str/blank? qs)
    {}
    (->> (str/split qs #"&")
         (keep (fn [pair]
                 (let [[k v] (str/split pair #"=" 2)]
                   (when (and k v)
                     [(keyword k)
                      (java.net.URLDecoder/decode v "UTF-8")]))))
         (into {}))))

(defn- handle-dashboard
  "GET / — server-rendered HTML dashboard."
  [req]
  (let [q (parse-query (:query-string req))]
    {:status 200
     :headers {"Content-Type" "text/html; charset=utf-8"}
     :body (dashboard-html q)}))

(defn- handle-health
  [hooks]
  {:status 200
   :headers {"Content-Type" "application/json"}
   :body (json/generate-string
           {:status "ok"
            :hooks (mapv (fn [[n h]] {:name n :ns (:ns h) :description (:description h)})
                         (sort-by first hooks))})})

(def ^:private debug-html
  "Bare-minimum HTML: no external CSS, no Pico, no Google Fonts, no JS,
  no meta-refresh, no form handlers. If this is interactive in the
  user's browser but / is not, the dashboard's CSS/HTML is the culprit.
  If neither is interactive, something in the browser environment is."
  "<!doctype html>
<html><head><meta charset=\"utf-8\"><title>cch debug</title></head>
<body style=\"font-family:sans-serif;padding:2em;max-width:40em\">
<h1>cch interactivity test</h1>
<p>This page has zero external assets, zero JS, zero custom CSS. If clicks/hovers don't work <em>here</em>, the issue is in your browser environment, not the dashboard CSS.</p>

<h2>1. Native &lt;details&gt;</h2>
<details><summary>Click to expand</summary><p>If you're reading this, clicking worked.</p></details>

<h2>2. Link hover</h2>
<p><a href=\"#\" title=\"If you can see this tooltip on hover, hovers work\">Hover me</a> (native tooltip via title attr)</p>

<h2>3. Form (no JS)</h2>
<form method=\"get\" action=\"/debug\">
<label>Type something: <input type=\"text\" name=\"q\" value=\"\"></label>
<button type=\"submit\">Submit</button>
</form>

<h2>4. Select</h2>
<form method=\"get\" action=\"/debug\">
<label>Pick: <select name=\"x\" onchange=\"this.form.submit()\">
<option value=\"\">–</option>
<option value=\"a\">A</option>
<option value=\"b\">B</option>
</select></label>
</form>

<hr>
<p><a href=\"/\">&larr; back to dashboard</a></p>
</body></html>")

(defn- route
  "Top-level request router. Reads path + method; dispatches."
  [hooks req]
  (let [{:keys [request-method uri]} req
        [_ hook-path] (re-matches #"/hooks/(.+)" uri)]
    (cond
      (and (= request-method :post) hook-path)
      (handle-hook-dispatch hooks hook-path req)

      (and (= request-method :get) (= uri "/health"))
      (handle-health hooks)

      (and (= request-method :get) (= uri "/debug"))
      {:status 200
       :headers {"Content-Type" "text/html; charset=utf-8"}
       :body debug-html}

      (and (= request-method :get) (= uri "/"))
      (handle-dashboard req)

      :else
      {:status 404 :body "not found"})))

;; --- Lifecycle ---

(defn start!
  "Start the httpkit server. Returns a stop-fn that gracefully shuts it down."
  [{:keys [port host] :or {port 8888 host "127.0.0.1"}}]
  (let [hooks (build-registry)
        stop-fn (httpkit/run-server (fn [req] (route hooks req))
                                    {:port port :ip host})]
    (println (format "cch serve listening on http://%s:%d (%d hook(s) loaded)"
                     host port (count hooks)))
    (doseq [[n h] (sort-by first hooks)]
      (println (format "  → /hooks/%-14s %s" n (:description h))))
    (println)
    (println (format "Dashboard:  http://%s:%d/" host port))
    {:stop stop-fn :hooks hooks}))

(defn -main
  "Foreground server with graceful shutdown."
  [& args]
  (let [port  (or (some->> args (drop-while #(not= "--port" %)) second Long/parseLong) 8888)
        host  (or (some->> args (drop-while #(not= "--host" %)) second) "127.0.0.1")
        {:keys [stop]} (start! {:port port :host host})
        latch (promise)]
    (.addShutdownHook (Runtime/getRuntime)
                      (Thread. (fn []
                                 (println "\ncch serve: shutting down")
                                 (stop :timeout 100)
                                 (deliver latch :stop))))
    @latch))
