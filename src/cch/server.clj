(ns cch.server
  "Long-lived HTTP dispatcher for cch hooks.

  Runs on localhost and exposes:

    POST /dispatch/<event> → fan out to every :code hook in the registry
                             that subscribes to <event>, matches the
                             tool_name (for tool events), and is enabled
                             in effective config. Reconcile their results
                             into a single protocol-shaped response.
    GET  /                 → server-rendered dashboard HTML (events + filters)
    GET  /health           → liveness + registered hooks (JSON)

  Dispatch goes to the same `composed` handler command mode uses
  (defined by defhook in cch.core). Logging/timing/error semantics are
  identical whether a hook is invoked via HTTP or directly.

  Dashboard is server-rendered via hiccup, styled with Pico.css +
  Google Fonts (Roboto / Roboto Condensed). No client JS — filter
  changes are plain GET form submits."
  (:require [babashka.fs :as fs]
            [babashka.nrepl.server :as nrepl]
            [cch.config :as config]
            [cch.config-db :as cdb]
            [cch.events :as events]
            [cch.log :as log]
            [cch.protocol :as proto]
            [cheshire.core :as json]
            [cli.registry :as registry]
            [clojure.java.shell :as shell]
            [clojure.string :as str]
            [hiccup2.core :as hic]
            [org.httpkit.server :as httpkit]))

;; --- Hook registration ---

(defn- load-hook
  "Require a hook's namespace and return {:name :composed-var :description
  :events [{:event :matcher} ...]}. Returns nil if the namespace can't
  load. Only :type :code entries are loaded — prompt/agent run natively.

  We hold the *var* (not its current value) so nREPL redefs of the
  hook's `composed` show up on the next dispatch — call sites must
  deref via @composed-var to get the live fn."
  [hook-name {:keys [ns description] :as reg-entry}]
  (when (= :code (registry/hook-type reg-entry))
    (try
      (require (symbol ns))
      (let [composed-var (ns-resolve (symbol ns) 'composed)]
        (when composed-var
          {:name         hook-name
           :ns           ns
           :description  description
           :composed-var composed-var
           :events       (registry/hook-events reg-entry)}))
      (catch Exception e
        (binding [*out* *err*]
          (println (format "cch.server: failed to load hook '%s' (%s): %s"
                           hook-name ns (.getMessage e))))
        nil))))

(defn- build-registry
  "Load every :code hook in cli.registry that resolves. Returns name→entry map."
  []
  (into {} (keep (fn [[n h]] (some-> (load-hook n h) (as-> e [n e])))
                 (registry/list-hooks))))

(defn- build-event-index
  "From the loaded-hooks map, build {event-name → [{:name :composed-var :matcher} ...]}
  so a request for event E can find its candidate hooks in one lookup.
  Holds the var, not the value — the dispatcher deref's it per call so
  nREPL redefs are picked up live."
  [hooks]
  (reduce-kv
    (fn [idx name {:keys [composed-var events]}]
      (reduce (fn [i {:keys [event matcher]}]
                (update i event (fnil conj [])
                        {:name         name
                         :composed-var composed-var
                         :matcher      matcher}))
              idx events))
    {}
    hooks))

;; --- Dashboard (server-rendered HTML via hiccup) ---

(defn- git-out
  "Run a git subcommand in cwd; return trimmed stdout on exit 0, nil otherwise."
  [cwd & args]
  (try
    (let [result (apply shell/sh "git" "-C" (or cwd ".") args)]
      (when (zero? (:exit result))
        (str/trim (:out result))))
    (catch Exception _ nil)))

(defn- parse-origin-name
  "Pull a repo name out of a remote URL.
    git@github.com:user/repo.git → 'repo'
    https://github.com/user/repo  → 'repo'
  Strips a trailing .git. Returns nil if we can't find anything."
  [url]
  (when-not (str/blank? url)
    (let [stripped (-> url str/trim (str/replace #"\.git/?$" "") (str/replace #"/$" ""))
          last-seg (last (str/split stripped #"[/:]"))]
      (when-not (str/blank? last-seg) last-seg))))

(defonce ^:private git-meta-cache (atom {}))
(def ^:private git-meta-ttl-ms 30000)

(defn git-meta
  "Resolve {:root, :repo-name, :branch, :in-repo?} for a cwd. Caches
  for 30s so renders + streams don't re-shell-out per event, but
  branch switches become visible without restarting the server.

  :root       — worktree root path; falls back to cwd when git fails
                (cwd moved, deleted, or never was a repo)
  :repo-name  — origin-derived repo identity (last segment of the URL,
                sans .git). nil when the cwd has no remote.origin.url
                (bare checkouts, fresh inits, deleted dirs) — callers
                fall back to a path-based label.
  :branch     — HEAD's abbrev-ref (e.g. 'main', 'feat/x'); nil if
                detached or not in a repo
  :in-repo?   — true only if git rev-parse --show-toplevel succeeded;
                lets callers distinguish 'git repo with no origin' from
                'not in any repo at all'"
  [cwd]
  (let [now (System/currentTimeMillis)
        entry (get @git-meta-cache cwd)]
    (if (and entry (< (- now (:at entry)) git-meta-ttl-ms))
      (:meta entry)
      (let [toplevel (git-out cwd "rev-parse" "--show-toplevel")
            root     (or toplevel cwd)
            origin   (git-out cwd "config" "--get" "remote.origin.url")
            branch   (let [b (git-out cwd "rev-parse" "--abbrev-ref" "HEAD")]
                       (when (and b (not= "HEAD" b)) b))
            meta     {:root      root
                      :repo-name (parse-origin-name origin)
                      :branch    branch
                      :in-repo?  (some? toplevel)}]
        (swap! git-meta-cache assoc cwd {:at now :meta meta})
        meta))))

(defn- worktree-root-of
  "Back-compat shim — just the :root from git-meta. Used by distinct-repos
  and the hook matrix's scope-to-cwd resolver."
  [cwd]
  (:root (git-meta cwd)))

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
  "All distinct cwds from the events table. SQL DISTINCT via
  log/distinct-cwds — avoids pulling the full events table just to
  populate the Repo dropdown."
  []
  (log/distinct-cwds))

(defn- repo-dropdown-label
  "Dropdown label for a cwd: prefer origin-derived repo name. When this
  is a linked worktree (worktree basename differs from repo-name), suffix
  with ' · <worktree>' so linked copies stay distinguishable. Non-origin
  repos fall back to the path-based repo-label."
  [cwd]
  (let [{:keys [root repo-name]} (git-meta cwd)
        wt-name (last (remove str/blank? (str/split (or root "") #"/")))]
    (cond
      (not repo-name)             (repo-label root)
      (= repo-name wt-name)       repo-name
      :else                       (str repo-name " · " wt-name))))

(defn- distinct-repos
  "Distinct cwds from events → worktree roots → filtered → [value label] pairs.
  Value is the worktree root (used by the cwd-prefix filter). Drops
  ephemeral tmp paths AND paths that aren't inside any git repo so the
  Repo dropdown only lists actual repos."
  []
  (let [cwds  (distinct-cwds)
        roots (->> cwds
                   (filter #(:in-repo? (git-meta %)))
                   (map worktree-root-of)
                   (remove ephemeral-path?)
                   (into (sorted-set)))]
    (map (fn [r] [r (repo-dropdown-label r)]) roots)))

(defn- select-with-options
  "Render a <select> from [[value label] ...] pairs, marking `current`
  selected. Auto-submits its parent form on change so filter changes
  apply immediately — no Apply button needed."
  [name current options]
  [:select {:name name :onchange "this.form.submit()"}
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

(def ^:private nav-hook-svg
  "Inline version of the favicon glyph, sized for the nav brand. Uses
  currentColor so CSS in .nav-icon controls its color."
  [:svg {:xmlns "http://www.w3.org/2000/svg"
         :width 22 :height 22 :viewBox "0 0 24 24"
         :fill "none" :stroke "currentColor" :stroke-width 2
         :stroke-linecap "round" :stroke-linejoin "round"
         :aria-hidden "true"}
   [:path {:d "m17.586 11.414-5.93 5.93a1 1 0 0 1-8-8l3.137-3.137a.707.707 0 0 1 1.207.5V10"}]
   [:path {:d "M20.414 8.586 22 7"}]
   [:circle {:cx 19 :cy 10 :r 2}]])

(defn- nav-bar
  "Primary nav at the top of every page. `active` is :events or :hooks —
  that tab renders as a non-link active label; the other renders as an
  ordinary link."
  [active]
  [:div.nav-wrap
   [:div.nav-title
    [:span.nav-icon nav-hook-svg]
    [:h1.nav-brand "cch"]]
   [:div.tabs
    [:ul
     [:li {:class (when (= active :events) "is-active")}
      (if (= active :events) [:a "events"] [:a {:href "/"} "events"])]
     [:li {:class (when (= active :hooks) "is-active")}
      (if (= active :hooks) [:a "hooks"] [:a {:href "/hooks"} "hooks"])]]]])

(defn- badge
  [hook-name]
  (let [{:keys [color tooltip]} (get hook-metadata hook-name)]
    [:span.tag.is-light
     (cond-> {:style (str "background:" (or color "#495057") ";color:white;")}
       tooltip (assoc :title tooltip))
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
  full detail on expand. When `open-all?` is true, renders <details open>
  so every card starts expanded — useful for scanning many events' full
  payloads at once after filtering down."
  [open-all? {:keys [id timestamp session_id hook_name event_type tool_name
                     file_path cwd decision reason elapsed_ms extra]}]
  (let [observation? (and (= hook_name "event-log") (nil? decision))
        short-ts     (apply str (take 19 (or timestamp "")))
        {:keys [root repo-name branch in-repo?]} (when cwd (git-meta cwd))
        ;; Prefer origin-derived name + branch. If origin isn't set (bare
        ;; checkout or fresh init), use the path-based repo-label (which
        ;; handles `foo-worktrees/bar` layouts). Outside any repo, mark
        ;; the row as "(no repo)" rather than pretending.
        repo-display (cond
                       (and repo-name branch)  (str repo-name " · " branch)
                       repo-name               repo-name
                       (and in-repo? root)     (repo-label root)
                       cwd                     "(no repo)"
                       :else                   "—")
        ;; Trailing context cell: prefer the hook's reason (when a
        ;; decision fired), else the basename of the file being
        ;; touched, else nothing. Full values live in the expanded
        ;; detail so the summary stays skimmable.
        file-basename (when-not (str/blank? file_path)
                        (last (remove str/blank? (str/split file_path #"/"))))
        context      (cond
                       (not (str/blank? reason)) (apply str (take 100 reason))
                       file-basename             file-basename
                       :else                     "")]
    [:article.event {:class (if observation? "observed" "acted")}
     [:details (when open-all? {:open "open"})
      ;; summary is itself the CSS grid. The chevron is a real span (not
      ;; ::before + ::marker) so browser default markers can't double up.
      [:summary
       [:span.chev "▸"]
       [:span.ts short-ts]
       [:span.repo {:title cwd} repo-display]
       (badge hook_name)
       [:span.evt event_type]
       [:span.tool (dash tool_name)]
       [:span.ctx {:title file_path} context]]
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
  "Return up to 30 most-recently-active session IDs as [[id label]...]
  pairs, optionally scoped to a cwd prefix. SQL does the GROUP BY +
  ORDER BY so we receive only ~30 rows rather than 2000. Label is
  'YYYY-MM-DDTHH:MM · <uuid-prefix>…'."
  [cwd-prefix]
  (->> (log/recent-sessions :limit 30
                            :cwd-prefix (when-not (str/blank? cwd-prefix)
                                          cwd-prefix))
       (map (fn [{:keys [session_id timestamp]}]
              [session_id
               (format "%s · %s…"
                       (apply str (take 16 (or timestamp "")))
                       (apply str (take 8 session_id)))]))))

(defn- select-field
  "One Bulma .field column wrapping a labeled <select>."
  [label name current options]
  [:div.column
   [:div.field
    [:label.label.is-small label]
    [:div.control
     [:div.select.is-small.is-fullwidth
      (select-with-options name current options)]]]])

(defn- input-field
  "One Bulma .field column wrapping a labeled <input>."
  [label input-map]
  [:div.column
   [:div.field
    [:label.label.is-small label]
    [:div.control
     [:input.input.is-small input-map]]]])

(defn- filter-form
  [q repos]
  [:form.filters {:method "get"}
   [:div.columns
    (select-field "Repo"     "cwd-prefix" (:cwd-prefix q "")
                  (cons ["" "all"] repos))
    (select-field "Hook"     "hook"       (:hook q "")
                  [["" "all"]
                   ["event-log" "event-log"]
                   ["scope-lock" "scope-lock"]
                   ["command-audit" "command-audit"]])
    (select-field "Event"    "event"      (:event q "")
                  (cons ["" "all"]
                        (for [e (hook-scoped-events (:hook q))] [e e])))
    (select-field "Decision" "decision"   (:decision q "")
                  [["" "all"] ["allow" "allow"] ["ask" "ask"]
                   ["deny" "deny"] ["block" "block"]])]
   [:div.columns
    (select-field "Session" "session" (:session q "")
                  (cons ["" "all"] (distinct-sessions (:cwd-prefix q))))
    ;; Text/number inputs still need an explicit trigger — submit on Enter.
    (input-field  "Since (SQLite ts)"
                  {:type "text" :name "since" :value (:since q "")
                   :placeholder "2026-04-13"})
    (input-field  "Limit"
                  {:type "number" :name "limit" :value (:limit q "50")
                   :min "1" :max "500"})]])

(def dashboard-css
  ":root {
     --bulma-family-primary: 'Inter', system-ui, sans-serif;
     --bulma-family-secondary: 'Inter', system-ui, sans-serif;
     --bulma-family-code: 'JetBrains Mono', ui-monospace, monospace;
     --bulma-body-family: var(--bulma-family-primary);
   }
   @media (prefers-color-scheme: dark) {
     :root:not([data-theme=\"light\"]) {
       color-scheme: dark;
       --bulma-scheme-main: hsl(221, 14%, 10%);
       --bulma-scheme-main-bis: hsl(221, 14%, 13%);
       --bulma-scheme-main-ter: hsl(221, 14%, 16%);
       --bulma-scheme-invert: hsl(221, 14%, 96%);
       --bulma-scheme-invert-bis: hsl(221, 14%, 92%);
       --bulma-background: hsl(221, 14%, 16%);
       --bulma-body-background-color: var(--bulma-scheme-main);
       --bulma-body-color: hsl(221, 14%, 86%);
       --bulma-text: hsl(221, 14%, 86%);
       --bulma-text-weak: hsl(221, 14%, 66%);
       --bulma-text-strong: hsl(221, 14%, 96%);
       --bulma-border: hsl(221, 14%, 22%);
       --bulma-border-weak: hsl(221, 14%, 18%);
       --bulma-code-background: hsl(221, 14%, 13%);
       --bulma-code: hsl(221, 14%, 86%);
       --bulma-pre-background: hsl(221, 14%, 13%);
       --bulma-link: hsl(209, 100%, 70%);
       --bulma-link-hover: hsl(209, 100%, 80%);
     }
   }

   /* Inter covers body + display; give headings weight and tighter tracking
      for rhythm without pulling in a second face. */
   h1, h2, h3, h4, h5, h6, th { font-family: var(--bulma-family-primary); font-weight: 600; letter-spacing: -0.01em; }
   h1 { margin-bottom: 0.2em; letter-spacing: -0.02em; }

   /* Top nav: hook glyph + product name on the left, tabs on the right.
      Tabs component from Bulma handles active-state styling; we only
      need layout. */
   .nav-wrap { display: flex; align-items: center; gap: 1.5em; margin-bottom: 0.6em; }
   .nav-title { display: flex; align-items: center; gap: 0.45em; }
   .nav-icon { color: #059669; display: inline-flex; align-items: center; }
   .nav-icon svg { display: block; }
   .nav-brand { font-size: 1.4em; font-weight: 700; letter-spacing: -0.02em; color: var(--bulma-text-strong); margin: 0; }
   .nav-wrap .tabs { margin-bottom: 0 !important; flex: 1; }
   .nav-wrap .tabs ul { border-bottom: none; }
   .subtitle { color: var(--bulma-text-weak); margin-top: 0; }
   .meta { color: var(--bulma-text-weak); font-size: 0.85em; }

   /* --- Event list (bespoke grid — Bulma doesn't express this shape) --- */
   .event-list article.event { margin: 0; padding: 0; border-radius: 4px; border: none; background: transparent; }
   .event-list article.event + article.event { margin-top: 2px; }
   .event-list details { margin: 0; }

   .event-list details summary {
     display: grid;
     grid-template-columns: 1em 11em 20em 7em 10em 6em minmax(10em, 1fr);
     gap: 0.7em;
     align-items: center;
     cursor: pointer;
     padding: 0.35em 0.6em;
     border-radius: 4px;
     font-size: 0.88em;
     list-style: none;
   }
   .event-list details summary::-webkit-details-marker { display: none; }
   .event-list details summary::marker { display: none; }
   .event-list details summary::after { display: none; }
   .event-list details summary .chev {
     color: var(--bulma-text-weak);
     transition: transform 0.1s;
     font-size: 0.9em;
     display: inline-block;
   }
   .event-list details[open] summary .chev { transform: rotate(90deg); }
   .event-list details summary:hover { background: var(--bulma-background); }
   .event-list article.observed summary { opacity: 0.65; }
   .event-list article.acted summary { font-weight: 500; }

   .event-list details summary > *:not(.tag) { overflow: hidden; text-overflow: ellipsis; white-space: nowrap; min-width: 0; }
   .event-list details summary .tag { overflow: visible; }
   .event-list details summary .ts { font-family: var(--bulma-family-code); color: var(--bulma-text-weak); }
   .event-list details summary .repo { color: var(--bulma-text-weak); }
   .event-list details summary .evt { font-weight: 500; }
   .event-list details summary .ctx { color: var(--bulma-text-weak); font-style: italic; cursor: help; }

   .event-list .detail { padding: 0.8em 1.2em 1em 2em; background: var(--bulma-background); border-radius: 0 0 4px 4px; font-size: 0.9em; }
   .event-list .detail dl { display: grid; grid-template-columns: max-content 1fr; gap: 0.3em 1em; margin: 0 0 1em 0; }
   .event-list .detail dt { font-family: var(--bulma-family-secondary); color: var(--bulma-text-weak); font-size: 0.85em; margin: 0; }
   .event-list .detail dd { margin: 0; font-family: var(--bulma-family-code); font-size: 0.85em; word-break: break-all; }
   .event-list .detail h5 { margin: 1em 0 0.3em 0; font-size: 0.85em; color: var(--bulma-text-weak); }
   .event-list .detail pre { font-size: 0.8em; white-space: pre-wrap; word-break: break-word; max-height: 30em; overflow: auto; background: var(--bulma-scheme-main-bis); padding: 0.7em; border-radius: 4px; margin: 0; }

   /* Filter form spacing under Bulma .columns. */
   .filters { margin: 1.5em 0; }")

(defn- encode-query
  "Build a `?k=v&...` query string from a keyword-keyed map, dropping
  blank values. URL-encodes values."
  [m]
  (let [pairs (for [[k v] m
                    :when (and v (not (str/blank? (str v))))]
                (str (name k) "="
                     (java.net.URLEncoder/encode (str v) "UTF-8")))]
    (if (seq pairs)
      (str "?" (str/join "&" pairs))
      "")))

(defn- dashboard-html
  [q]
  (let [open-all? (= "all" (:open q ""))
        ;; Query used when following an in-page link — preserves filters
        ;; but toggles the open flag.
        link-q    (dissoc q :open)
        open-url  (str "/" (encode-query (assoc link-q :open "all")))
        close-url (str "/" (encode-query link-q))
        self-url  (str "/" (encode-query q))
        events    (log/query-events
                    :limit       (or (some-> (:limit q) Long/parseLong) 50)
                    :hook        (when-not (str/blank? (:hook q)) (:hook q))
                    :event       (when-not (str/blank? (:event q)) (:event q))
                    :session     (when-not (str/blank? (:session q)) (:session q))
                    :decision    (when-not (str/blank? (:decision q)) (:decision q))
                    :since       (when-not (str/blank? (:since q)) (:since q))
                    :cwd-prefix  (when-not (str/blank? (:cwd-prefix q)) (:cwd-prefix q)))
        repos     (distinct-repos)]
    (str "<!doctype html>\n"
         (hic/html
           [:html {:lang "en"}
            [:head
             [:meta {:charset "utf-8"}]
             [:title "cch · events"]
             [:meta {:name "viewport" :content "width=device-width,initial-scale=1"}]
             ;; No auto-refresh: it kills any <details open> state the user
             ;; has opened and jumps the scroll position. Manual refresh
             ;; (Cmd+R, or the link in the meta area) until we have
             ;; something smarter like SSE-patched partial reloads.
             [:link {:rel "icon" :type "image/svg+xml" :href "/favicon.svg"}]
             [:link {:rel "preconnect" :href "https://fonts.googleapis.com"}]
             [:link {:rel "preconnect" :href "https://fonts.gstatic.com" :crossorigin true}]
             [:link {:rel "stylesheet"
                     :href "https://fonts.googleapis.com/css2?family=Inter:wght@400;500;600&family=JetBrains+Mono:wght@400;500&display=swap"}]
             [:link {:rel "stylesheet"
                     :href "https://cdn.jsdelivr.net/npm/bulma@1.0.2/css/bulma.min.css"}]
             [:script {:type "module"
                       :src  "https://cdn.jsdelivr.net/gh/starfederation/datastar@v1.0.0-RC.8/bundles/datastar.js"}]
             [:style (hic/raw dashboard-css)]]
            [:body
             [:section.section
              [:div.container
               (nav-bar :events)
               [:p.subtitle
                "Centralized log of every Claude Code event cch is subscribed to. "
                "Refresh the page (Cmd+R) to load newer events."]
               (filter-form q repos)
               [:p.meta
                (format "%d event(s) · " (count events))
                [:a {:href self-url} "↻ refresh"]
                " · "
                (if open-all?
                  [:a {:href close-url} "close all"]
                  [:a {:href open-url} "open all"])
                " · "
                [:a {:href "/"} "clear filters"]]
               [:div.event-list
                {:data-on-load "@get('/events/stream')"}
                (for [e events] (event-card open-all? e))]]]]]))))

;; --- Handlers ---

(defn- reconcile
  "Combine multiple hook results into one.

  Precedence (first match wins):
    1. :decision :deny
    2. :decision :ask
    3. :decision :allow with :updated-input (PreToolUse-only semantic)
    4. concatenated :context across all results (events that support
       additionalContext; the protocol renderer decides whether this
       shape emits anything for the event)
    5. nil"
  [results]
  (let [non-nil (remove nil? results)]
    (or (first (filter #(= :deny  (:decision %)) non-nil))
        (first (filter #(= :ask   (:decision %)) non-nil))
        (first (filter #(and (= :allow (:decision %)) (:updated-input %)) non-nil))
        (let [contexts (keep :context non-nil)]
          (when (seq contexts)
            {:context (str/join "\n\n" contexts)})))))

(defn- run-hook
  "Invoke a single hook's composed handler with the input, catching and
  logging exceptions. Returns the decision map (or nil).

  Deref's :composed-var per call so nREPL redefs of a hook's `composed`
  show up immediately on the next dispatch."
  [{:keys [name composed-var]} input event]
  (try
    (@composed-var (assoc input :cch/hook-name name))
    (catch Exception e
      (binding [*out* *err*]
        (println (format "cch.server: hook '%s' on event '%s' threw: %s"
                         name event (.getMessage e))))
      nil)))

(defn- applicable-hooks
  "Filter an event's candidate hooks down to those enabled in effective
  config and whose matcher matches the tool_name (if any)."
  [candidates tool-name effective-config]
  (filter (fn [{:keys [name matcher]}]
            (and (get-in effective-config [:hooks name :enabled?] false)
                 (registry/matcher-matches? matcher tool-name)))
          candidates))

(defn- handle-dispatch
  "POST /dispatch/<event> — fan out to matching enabled :code hooks,
  reconcile, serialize."
  [event-idx event req]
  (try
    (let [body-str  (slurp (:body req))
          input     (json/parse-string body-str true)
          cwd       (:cwd input)
          effective (config/load-effective-config cwd)
          candidates (get event-idx event [])
          apps      (applicable-hooks candidates (:tool_name input) effective)
          results   (mapv #(run-hook % input event) apps)
          reconciled (reconcile results)
          json-out  (proto/->response event reconciled)]
      {:status  200
       :headers {"Content-Type" "application/json"}
       :body    (or json-out "")})
    (catch Exception e
      (binding [*out* *err*]
        (println (format "cch.server: /dispatch/%s error: %s" event (.getMessage e))))
      {:status  500
       :headers {"Content-Type" "application/json"}
       :body    (json/generate-string {:error (.getMessage e)})})))

(defn- parse-query
  "Parse httpkit's :query-string into a keyword-keyed map. {} when blank."
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

;; --- Favicon ---

(def ^:private favicon-svg
  "Lucide 'fishing-hook' glyph, stroke fixed to the same emerald we use
  for enabled toggles so it reads on both light and dark tab bars."
  (str "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
       "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"24\" height=\"24\" "
       "viewBox=\"0 0 24 24\" fill=\"none\" stroke=\"#059669\" stroke-width=\"2\" "
       "stroke-linecap=\"round\" stroke-linejoin=\"round\">"
       "<path d=\"m17.586 11.414-5.93 5.93a1 1 0 0 1-8-8l3.137-3.137a.707.707 0 0 1 1.207.5V10\"/>"
       "<path d=\"M20.414 8.586 22 7\"/>"
       "<circle cx=\"19\" cy=\"10\" r=\"2\"/>"
       "</svg>"))

(defn- handle-favicon [_req]
  {:status  200
   :headers {"Content-Type"  "image/svg+xml"
             "Cache-Control" "public, max-age=86400"}
   :body    favicon-svg})

;; --- Live event stream (Datastar + SSE) ---

(defn- event-fragment-frame
  "Format one event as a Datastar patch-elements SSE frame (v1 protocol).
  Selector is '.event-list'; mode 'prepend' inserts the new card at the
  top while leaving existing cards (and any open <details>) untouched.

  SSE field values cannot contain raw newlines (\\n terminates a field
  per spec), so we substitute HTML numeric-entity &#10; for every
  newline in the rendered fragment. Inside <pre> blocks the browser
  still renders those entities as real line breaks — pretty-printed
  JSON payloads look right in the expanded detail view."
  [event]
  (let [html (str/replace (str (hic/html (event-card false event)))
                          "\n" "&#10;")]
    (str "event: datastar-patch-elements\n"
         "data: selector .event-list\n"
         "data: mode prepend\n"
         "data: elements " html "\n"
         "\n")))

(def ^:private sse-heartbeat-ms
  "Interval between keep-alive comment frames so proxies / browsers
  don't close idle SSE connections."
  15000)

(defn- handle-event-stream
  "GET /events/stream — opens a long-lived SSE connection. Each new
  hook invocation (via events/publish! from wrap-logging) is sent as
  a Datastar merge-fragments frame that prepends a card to .event-list."
  [req]
  (let [unsub-ref (atom nil)
        heartbeat (atom nil)]
    (httpkit/as-channel
      req
      {:on-open
       (fn [ch]
         ;; First send establishes the response + SSE headers. No body
         ;; yet — some SSE clients (notably Datastar) don't like a
         ;; comment-only preamble before the first real event.
         (httpkit/send! ch
                        {:status  200
                         :headers {"Content-Type"      "text/event-stream"
                                   "Cache-Control"     "no-cache"
                                   "X-Accel-Buffering" "no"}}
                        false)
         ;; Subscribe: each published event becomes an SSE frame.
         (reset! unsub-ref
                 (events/subscribe!
                   (fn [event]
                     (try
                       (httpkit/send! ch (event-fragment-frame event) false)
                       (catch Exception _ nil)))))
         ;; Heartbeat: an SSE comment every 15s so middle boxes don't
         ;; reap us as idle. httpkit sends are no-op on closed channels.
         (reset! heartbeat
                 (future
                   (try
                     (loop []
                       (Thread/sleep sse-heartbeat-ms)
                       (when (httpkit/send! ch ": heartbeat\n\n" false)
                         (recur)))
                     (catch InterruptedException _ nil)))))
       :on-close
       (fn [_ _]
         (when-let [u @unsub-ref] (u))
         (when-let [hb @heartbeat] (future-cancel hb)))})))

;; --- Config CRUD (JSON API) ---

(defn- handle-config-list
  "GET /api/config — return all hook_config rows."
  [_req]
  {:status  200
   :headers {"Content-Type" "application/json"}
   :body    (json/generate-string (or (cdb/list-all) []))})

(defn- parse-body
  "Parse request body as JSON. Returns {} on blank/malformed."
  [req]
  (try
    (let [s (slurp (:body req))]
      (if (str/blank? s) {} (json/parse-string s true)))
    (catch Exception _ {})))

(defn- parse-form
  "Parse application/x-www-form-urlencoded body into a keyword-keyed map."
  [req]
  (let [s (slurp (:body req))]
    (if (str/blank? s)
      {}
      (->> (str/split s #"&")
           (keep (fn [pair]
                   (let [[k v] (str/split pair #"=" 2)]
                     (when (and k v)
                       [(keyword (java.net.URLDecoder/decode k "UTF-8"))
                        (java.net.URLDecoder/decode v "UTF-8")]))))
           (into {})))))

(defn- content-type [req]
  (or (get-in req [:headers "content-type"])
      (get-in req [:headers "Content-Type"])
      ""))

(defn- form? [req]
  (str/includes? (content-type req) "application/x-www-form-urlencoded"))

(defn- truthy-str? [s]
  (contains? #{"true" "on" "1" "yes"} (str/lower-case (str s))))

(defn- handle-config-upsert
  "POST /api/config — upsert a row. Accepts JSON or form body.
  Required: hook (or hook-name), scope, enabled.
  Optional: options (JSON object)."
  [req]
  (try
    (let [body      (if (form? req) (parse-form req) (parse-body req))
          hook-name (or (:hook-name body) (:hook body))
          scope     (:scope body)
          enabled   (if (form? req)
                      (truthy-str? (:enabled body))
                      (boolean (:enabled body)))
          options   (:options body)]
      (if (or (str/blank? (str hook-name)) (str/blank? (str scope)))
        {:status 400
         :headers {"Content-Type" "application/json"}
         :body (json/generate-string {:error "hook and scope are required"})}
        (do
          (cdb/upsert! {:hook-name hook-name
                        :scope     scope
                        :enabled   enabled
                        :options   options})
          {:status  200
           :headers {"Content-Type" "application/json"}
           :body    (json/generate-string {:ok true
                                           :hook hook-name
                                           :scope scope
                                           :enabled enabled})})))
    (catch Exception e
      {:status  500
       :headers {"Content-Type" "application/json"}
       :body    (json/generate-string {:error (.getMessage e)})})))

(defn- handle-config-delete
  "DELETE /api/config?hook=X&scope=Y — remove a row."
  [req]
  (let [q (parse-query (:query-string req))
        hook-name (or (:hook q) (:hook-name q))
        scope     (:scope q)]
    (if (or (str/blank? (str hook-name)) (str/blank? (str scope)))
      {:status  400
       :headers {"Content-Type" "application/json"}
       :body    (json/generate-string {:error "hook and scope query params required"})}
      (do
        (cdb/delete! hook-name scope)
        {:status  200
         :headers {"Content-Type" "application/json"}
         :body    (json/generate-string {:ok true})}))))

;; --- Hook matrix page ---

(defn- effective-entry
  "Resolve the effective hook-config entry for one (hook, scope) pair.
  For global scope, reads directly from DB + defaults (no YAML). For repo
  scopes, runs the full load-effective-config to include YAML-authoritative
  precedence."
  [hook-name scope]
  (if (= scope cdb/global-scope)
    ;; Global: DB global row if present, else default
    (let [row (cdb/get-row hook-name cdb/global-scope)]
      (if row
        {:enabled? (:enabled row)
         :options  (:options row)
         :source   :db-global}
        {:enabled? true :options nil :source :default}))
    (let [repo-root (subs scope 5) ; strip "repo:"
          cfg (config/load-effective-config repo-root)]
      (get-in cfg [:hooks hook-name]
              {:enabled? true :options nil :source :default}))))

(defn- repo-yaml-present?
  "True if the repo at scope 'repo:/path' has a .cch-config.yaml."
  [scope]
  (when (str/starts-with? scope "repo:")
    (let [repo-root (subs scope 5)
          cfg (config/load-effective-config repo-root)]
      (some? (:yaml-path cfg)))))

(defn- all-scopes
  "Scopes to show as matrix columns: 'global' plus every repo cch has
  seen in events. Returns a seq of {:scope :label :yaml?}."
  []
  (let [repos (distinct-repos)]
    (cons {:scope cdb/global-scope :label "global" :yaml? false}
          (for [[path label] repos
                :let [scope (cdb/repo-scope path)]]
            {:scope scope :label label :yaml? (repo-yaml-present? scope)}))))

(def ^:private type-badge-colors
  {:code   "#6c757d"
   :prompt "#7c3aed"
   :agent  "#059669"})

(defn- type-badge [t]
  [:span.tag
   {:style (str "background:" (get type-badge-colors t "#495057") ";color:white;")}
   (name t)])

(defn- toggle-form
  "Render the enable/disable toggle form cell for (hook, scope)."
  [hook-name scope current-enabled? source yaml-managed?]
  (if yaml-managed?
    [:span.tag.is-warning.is-light
     {:title "managed by .cch-config.yaml — edit the file to change"}
     (if current-enabled? "✓ yaml" "✗ yaml")]
    [:form {:method "post" :action "/hooks/toggle" :class "toggle-form"}
     [:input {:type "hidden" :name "hook"    :value hook-name}]
     [:input {:type "hidden" :name "scope"   :value scope}]
     [:input {:type "hidden" :name "enabled" :value (if current-enabled? "false" "true")}]
     [:button.toggle {:type "submit"
                      :class (if current-enabled? "on" "off")
                      :data-source (name source)
                      :title (str "source: " (name source)
                                  " — click to "
                                  (if current-enabled? "disable" "enable"))}
      (if current-enabled? "on" "off")]]))

(def ^:private matrix-css
  "/* Scope-as-rows layout — first column is the scope label; every other
      column is a hook. Stacked column header: hook name over type badge. */
   table.matrix th .hook-head { display: flex; flex-direction: column; gap: 0.2em; align-items: flex-start; }
   table.matrix th .hook-name { font-weight: 600; font-size: 0.9em; }
   table.matrix th .hook-type { font-size: 0.7em; }
   table.matrix td.scope-name { font-weight: 500; font-family: var(--bulma-family-code); font-size: 0.85em; white-space: nowrap; }
   table.matrix td.scope-name .yaml-badge { color: var(--bulma-text-weak); font-style: italic; font-weight: 400; font-family: var(--bulma-family-primary); }
   table.matrix tr.scope-global { background: rgba(100, 100, 100, 0.05); }
   .toggle-form { display: inline; margin: 0; }
   button.toggle { padding: 2px 10px; font-size: 0.8em; border-radius: 3px; border: 1px solid var(--bulma-border); cursor: pointer; font-family: inherit; min-width: 3em; background: transparent; color: var(--bulma-text-weak); }
   button.toggle.on  { background: #059669; color: white; border-color: #059669; }
   button.toggle.on[data-source=\"db-repo\"]   { background: #047857; border-color: #047857; }
   button.toggle.on[data-source=\"repo-yaml\"] { background: #7c3aed; border-color: #7c3aed; }
   .cell-readonly { color: var(--bulma-text-weak); font-size: 0.85em; font-style: italic; }")

(defn- matrix-cell
  "Render one (scope, hook) cell."
  [scope yaml? {:keys [name entry]}]
  (let [t (registry/hook-type entry)]
    [:td
     (if (= :code t)
       (let [{:keys [enabled? source]} (effective-entry name scope)]
         (toggle-form name scope enabled? source yaml?))
       [:span.cell-readonly "native"])]))

(defn- matrix-row
  "One row per scope. First cell is the scope label; remaining cells are
  one per hook."
  [{:keys [scope label yaml?]} entries]
  [:tr {:class (when (= scope cdb/global-scope) "scope-global")}
   [:td.scope-name
    label
    (when yaml? [:span.yaml-badge " · yaml"])]
   (for [entry entries]
     (matrix-cell scope yaml? entry))])

(defn- hooks-matrix-html
  [_q]
  (let [scopes (all-scopes)
        entries (for [[n e] (registry/list-hooks)] {:name n :entry e})]
    (str "<!doctype html>\n"
         (hic/html
           [:html {:lang "en"}
            [:head
             [:meta {:charset "utf-8"}]
             [:title "cch · hooks"]
             [:meta {:name "viewport" :content "width=device-width,initial-scale=1"}]
             [:link {:rel "icon" :type "image/svg+xml" :href "/favicon.svg"}]
             [:link {:rel "preconnect" :href "https://fonts.googleapis.com"}]
             [:link {:rel "preconnect" :href "https://fonts.gstatic.com" :crossorigin true}]
             [:link {:rel "stylesheet"
                     :href "https://fonts.googleapis.com/css2?family=Inter:wght@400;500;600&family=JetBrains+Mono:wght@400;500&display=swap"}]
             [:link {:rel "stylesheet"
                     :href "https://cdn.jsdelivr.net/npm/bulma@1.0.2/css/bulma.min.css"}]
             [:style (hic/raw dashboard-css)]
             [:style (hic/raw matrix-css)]]
            [:body
             [:section.section
              [:div.container
               (nav-bar :hooks)
               [:p.subtitle
                "Enable or disable each hook per scope. "
                "Per-repo .cch-config.yaml files take precedence over DB rows — those cells are shown read-only."]
               [:p.meta
                [:a {:href "/hooks"} "↻ refresh"]]
               [:table.table.is-hoverable.is-fullwidth.matrix
                [:thead
                 [:tr
                  [:th "scope"]
                  (for [{:keys [name entry]} entries]
                    [:th {:title (:description entry)}
                     [:div.hook-head
                      [:div.hook-name name]
                      [:div.hook-type (type-badge (registry/hook-type entry))]]])]]
                [:tbody
                 (for [scope scopes]
                   (matrix-row scope entries))]]
               [:p.meta
                [:small
                 (format "%d hook(s) · %d scope(s)"
                         (count entries) (count scopes))]]]]]]))))

(defn- handle-hooks-page [req]
  (let [q (parse-query (:query-string req))]
    {:status  200
     :headers {"Content-Type" "text/html; charset=utf-8"}
     :body    (hooks-matrix-html q)}))

(defn- handle-hooks-toggle
  "POST /hooks/toggle — form endpoint. Upserts the row then redirects to /hooks."
  [req]
  (let [form (parse-form req)
        hook-name (:hook form)
        scope     (:scope form)
        enabled   (truthy-str? (:enabled form))]
    (when (and (not (str/blank? (str hook-name)))
               (not (str/blank? (str scope))))
      (cdb/upsert! {:hook-name hook-name :scope scope :enabled enabled}))
    {:status  303
     :headers {"Location" "/hooks"}
     :body    ""}))

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
  [hooks event-idx req]
  (let [{:keys [request-method uri]} req
        [_ dispatch-event] (re-matches #"/dispatch/(.+)" uri)]
    (cond
      (and (= request-method :post) dispatch-event)
      (handle-dispatch event-idx dispatch-event req)

      (and (= request-method :get) (= uri "/health"))
      (handle-health hooks)

      (and (= request-method :get)
           (contains? #{"/favicon.svg" "/favicon.ico"} uri))
      (handle-favicon req)

      (and (= request-method :get) (= uri "/events/stream"))
      (handle-event-stream req)

      (and (= request-method :get) (= uri "/api/config"))
      (handle-config-list req)

      (and (= request-method :post) (= uri "/api/config"))
      (handle-config-upsert req)

      (and (= request-method :delete) (= uri "/api/config"))
      (handle-config-delete req)

      (and (= request-method :get) (= uri "/hooks"))
      (handle-hooks-page req)

      (and (= request-method :post) (= uri "/hooks/toggle"))
      (handle-hooks-toggle req)

      (and (= request-method :get) (= uri "/debug"))
      {:status 200
       :headers {"Content-Type" "text/html; charset=utf-8"}
       :body debug-html}

      (and (= request-method :get) (= uri "/"))
      (handle-dashboard req)

      :else
      {:status 404 :body "not found"})))

;; --- Lifecycle ---

(defn- start-nrepl!
  "Start an in-process nREPL bound to 127.0.0.1:<port> and write a
  .nrepl-port file in cwd so editor tooling auto-discovers it. Returns
  the server map that babashka.nrepl.server/stop-server! consumes, or
  nil when port is nil/blank."
  [port]
  (when port
    (let [server (nrepl/start-server! {:host "127.0.0.1" :port port})]
      (try (spit ".nrepl-port" (str port))
           (catch Exception _ nil))
      (println (format "nREPL server: 127.0.0.1:%d  (.nrepl-port written)" port))
      server)))

(defn- stop-nrepl!
  "Best-effort shutdown — silences exceptions, deletes .nrepl-port."
  [server]
  (when server
    (try (nrepl/stop-server! server) (catch Exception _ nil))
    (try (fs/delete-if-exists ".nrepl-port") (catch Exception _ nil))))

(defn start!
  "Start the httpkit server. Returns a stop-fn that gracefully shuts it down.

  Also boots the cch.log background writer so dispatcher inserts ride
  the queued path (sub-millisecond) instead of forking sqlite3 per call.
  The returned :stop drains and closes the writer.

  When :nrepl-port is set (or arrives via -main's --nrepl flag), also
  starts an in-process nREPL on 127.0.0.1 for live re-eval of any ns.

  Default host '::' binds all interfaces dual-stack on Linux/macOS so
  both `localhost` (often resolves to IPv6 ::1 first) and 127.0.0.1
  work without the browser eating a connection-refused retry loop.
  Pass --host 127.0.0.1 via start args to restrict to IPv4 loopback."
  [{:keys [port host nrepl-port] :or {port 8888 host "::"}}]
  (registry/validate-registry!)
  (log/start-writer!)
  (let [hooks     (build-registry)
        event-idx (build-event-index hooks)
        nrepl     (start-nrepl! nrepl-port)
        stop-fn   (httpkit/run-server (fn [req] (route hooks event-idx req))
                                      {:port port :ip host})]
    (println (format "cch serve listening on http://%s:%d (%d code hook(s) loaded)"
                     host port (count hooks)))
    (doseq [[n h] (sort-by first hooks)]
      (println (format "  → %-16s %s" n (:description h))))
    (println)
    (println (format "Dispatcher: http://%s:%d/dispatch/<event>" host port))
    (println (format "Dashboard:  http://%s:%d/" host port))
    {:stop  (fn shutdown [& args]
              ;; httpkit's stop-fn takes &{:as opts}, e.g. (stop :timeout 100)
              (try (apply stop-fn args) (catch Exception _ nil))
              (stop-nrepl! nrepl)
              (log/stop-writer!))
     :hooks hooks
     :nrepl nrepl}))

(defn- parse-int-opt
  "Pull an integer-valued flag out of args; falls back to env-var if
  unset; nil if neither present."
  [args flag env-var]
  (let [v (or (some->> args (drop-while #(not= flag %)) second)
              (System/getenv env-var))]
    (when (and v (re-matches #"\d+" (str v)))
      (Long/parseLong (str v)))))

(defn -main
  "Foreground server with graceful shutdown.

  Flags:
    --port <n>     HTTP dispatcher port (default 8888)
    --host <h>     bind address (default :: dual-stack)
    --nrepl <n>    optional nREPL port; off when omitted.
                   Falls back to env CCH_NREPL_PORT."
  [& args]
  (let [port       (or (parse-int-opt args "--port"  "CCH_PORT")  8888)
        host       (or (some->> args (drop-while #(not= "--host" %)) second) "::")
        nrepl-port (parse-int-opt args "--nrepl" "CCH_NREPL_PORT")
        {:keys [stop]} (start! {:port port :host host :nrepl-port nrepl-port})
        latch (promise)]
    (.addShutdownHook (Runtime/getRuntime)
                      (Thread. (fn []
                                 (println "\ncch serve: shutting down")
                                 (stop :timeout 100)
                                 (deliver latch :stop))))
    @latch))
