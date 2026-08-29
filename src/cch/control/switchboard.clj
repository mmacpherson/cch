(ns cch.control.switchboard
  "Cloudflare Access-protected fleet view of the cch application. Provider-
  native UIs remain authoritative for transcripts, terminal control, and
  approvals."
  (:require [cch.control.broker-api :as broker]
            [cch.control.naming :as naming]
            [cch.control.usage-forecast :as usage-forecast]
            [cch.control.web-auth :as auth]
            [cch.usage :as usage]
            [cch.web :as web]
            [clojure.string :as str]
            [hiccup2.core :as hic])
  (:import [java.net URI URLDecoder URLEncoder]
           [java.nio.charset StandardCharsets]
           [java.time Instant ZoneId]
           [java.time.format DateTimeFormatter]))

(def ^:private security-headers
  {"Cache-Control" "no-store"
   "Content-Security-Policy"
   (str "default-src 'none'; style-src 'unsafe-inline'; "
        "form-action 'self'; base-uri 'none'; frame-ancestors 'none'")
   "Referrer-Policy" "no-referrer"
   "X-Content-Type-Options" "nosniff"
   "X-Frame-Options" "DENY"})

(defn- response
  ([status body]
   {:status status
    :headers (assoc security-headers
                    "Content-Type" "text/html; charset=utf-8")
    :body body})
  ([status headers body]
   {:status status :headers (merge security-headers headers) :body body}))

(defn- redirect
  ([location] (redirect location nil))
  ([location set-cookie]
   (response 303
             (cond-> {"Location" location
                      "Content-Type" "text/plain; charset=utf-8"}
               set-cookie (assoc "Set-Cookie" set-cookie))
             "")))

(defn- url-decode [value]
  (URLDecoder/decode (or value "") StandardCharsets/UTF_8))

(defn- url-encode [value]
  (URLEncoder/encode (str value) StandardCharsets/UTF_8))

(defn- parse-form [encoded]
  (if (str/blank? encoded)
    {}
    (->> (str/split encoded #"&")
         (map (fn [part]
                (let [[key value] (str/split part #"=" 2)]
                  [(keyword (url-decode key)) (url-decode value)])))
         (into {}))))

(defn- request-form [request]
  (when-not (str/starts-with?
              (or (get-in request [:headers "content-type"]) "")
              "application/x-www-form-urlencoded")
    (throw (ex-info "Form content type is required" {:type :invalid-form})))
  (parse-form (some-> request :body slurp)))

(defn- request-query [request]
  (parse-form (:query-string request)))

(defn- authorized-form-origin? [config request]
  (let [headers (:headers request)
        expected-origin (:origin config)
        expected-authority (.getAuthority (URI/create expected-origin))]
    (or (= expected-origin (get headers "origin"))
        (and (= "same-origin" (get headers "sec-fetch-site"))
             (= (str/lower-case expected-authority)
                (some-> (get headers "host") str/lower-case))))))

(defn- csrf! [config request identity form]
  (when-not (authorized-form-origin? config request)
    (throw (ex-info "Request origin is invalid"
                    {:type :invalid-csrf :reason :origin})))
  (when-not (auth/csrf-valid? config identity (:csrf form))
    (throw (ex-info "CSRF token is invalid"
                    {:type :invalid-csrf :reason :token})))
  true)

(defn- log-form-rejection! [error]
  (let [data (ex-data error)
        reason (or (:reason data) (:type data) :unknown)]
    (binding [*out* *err*]
      (println (str "cch.control.switchboard: rejected form: "
                    (name reason))))))

(defn- attention [session]
  (case (str/lower-case (or (:status session) "unknown"))
    ("waiting" "blocked" "needs-attention")
    {:key "attention" :label "Needs you"}

    ("working" "running" "active" "busy")
    {:key "working" :label "Working"}

    {:key "ready" :label "Ready"}))

(def ^:private hosted-css
  "
.hosted-page .page-header{align-items:center;margin-bottom:1.2em}
.hosted-page .page-header h1{margin:0}
.hosted-subtitle{color:var(--fg-muted);font-size:var(--font-sm);margin-top:.2em}
.hosted-signout{margin:0}.hosted-signout .btn{padding:3px 9px;font-size:var(--font-xs)}
.panel{background:var(--surface);border:1px solid var(--border);border-radius:6px;padding:1em;margin:0 0 1em}
.panel h2{font-size:1.05em;margin:0 0 .8em}.panel>.muted{margin-bottom:1em}
.summary{display:flex;gap:2em;flex-wrap:wrap}.summary strong{font:600 1.6em/1.2 var(--family-mono);display:block}.summary small{color:var(--fg-muted)}
.sessions,.forecast-grid{display:grid;grid-template-columns:repeat(auto-fit,minmax(270px,1fr));gap:.8em}
.session,.forecast-card{border:1px solid var(--border);border-radius:6px;padding:1em;min-width:0}
.session-top{display:flex;justify-content:space-between;gap:.7em;align-items:center}.route{font:var(--font-xs)/1.4 var(--family-mono);word-break:break-all;color:var(--fg-muted)}
.badge{border-radius:999px;padding:2px 8px;font-size:var(--font-xs);border:1px solid var(--border)}
.badge-attention{color:var(--c-ask);border-color:var(--c-ask);background:var(--c-ask-bg)}.badge-working{color:var(--c-allow);border-color:var(--c-allow);background:var(--c-allow-bg)}.badge-ready{color:var(--accent);border-color:var(--accent);background:var(--accent-soft)}
label{display:block;margin:0 0 .35em;color:var(--fg-muted);font-size:var(--font-sm)}select,textarea,.alias-form input[name=alias]{width:100%;border:1px solid var(--border);border-radius:4px;background:var(--bg);color:var(--fg);padding:.6em;font:inherit}.alias-form{margin-top:.8em}.alias-form input[name=alias]{min-width:0;flex:1}textarea{min-height:110px;resize:vertical}.fields{display:grid;grid-template-columns:1fr 2fr;gap:1em}
.session-details{border-top:1px solid var(--border);margin-top:.8em;padding-top:.6em}.session-details summary{color:var(--fg-muted);cursor:pointer;font-size:var(--font-sm)}.session-meta{display:grid;gap:.45em;margin-top:.7em}.session-meta small{display:block;color:var(--fg-muted)}
button{display:inline-flex;align-items:center;padding:5px 12px;border:1px solid var(--accent);border-radius:4px;background:var(--accent);color:var(--bg);font:inherit;cursor:pointer}button.secondary{background:var(--surface);border-color:var(--border);color:var(--fg)}.actions{display:flex;gap:.6em;align-items:center;margin-top:.8em}.notice{border-left:3px solid var(--accent);padding:.7em .9em;background:var(--surface);margin-bottom:1em}.error{border-color:var(--c-deny)}.status-delivered{border-color:var(--c-allow)}.status-failed,.status-expired{border-color:var(--c-deny)}.inline{display:inline}.provider{margin-top:.6em}.empty{text-align:center;padding:2em;color:var(--fg-muted)}
.fleet-hero{display:grid;grid-template-columns:minmax(250px,1.2fr) minmax(360px,1fr);gap:2em;align-items:center;padding:1.25em 1.4em;border-color:oklch(72% .18 235/.35);background:linear-gradient(120deg,var(--accent-soft),var(--surface) 52%);position:relative;overflow:hidden}.fleet-hero:after{content:'';position:absolute;right:-5em;top:-8em;width:18em;height:18em;border:1px solid oklch(72% .18 235/.13);border-radius:50%;pointer-events:none}.fleet-kicker{font:500 var(--font-xs)/1.3 var(--family-mono);letter-spacing:.08em;text-transform:uppercase;color:var(--accent);display:flex;align-items:center;gap:.55em;margin-bottom:.35em}.panel h2.fleet-title{font-size:1.45em;margin:0 0 .25em}.fleet-copy{color:var(--fg-muted);font-size:var(--font-sm);margin:0}.fleet-metrics{display:grid;grid-template-columns:repeat(3,1fr);position:relative;z-index:1}.fleet-metric{padding:.2em 1em;border-left:1px solid var(--border)}.fleet-metric strong{display:block;font:600 1.7em/1.2 var(--family-mono)}.fleet-metric small{color:var(--fg-muted)}
.session-context{display:flex;align-items:center;gap:.45em;margin-top:.15em;color:var(--fg-muted);font-size:var(--font-sm)}.session-context .runner-chip{color:var(--fg)}.context-separator{color:var(--border)}.recent-panel{padding:1.1em 1.2em}.recent-heading{display:flex;align-items:baseline;justify-content:space-between;margin-bottom:.7em}.recent-heading h2{margin:0}.live-label{display:inline-flex;align-items:center;gap:.45em;color:var(--fg-muted);font:var(--font-xs)/1 var(--family-mono);margin-left:.7em}.live-label .dot{background:var(--c-allow)}
.activity-table .event-time{width:13%;font-family:var(--family-mono);color:var(--fg-muted)}.activity-table .event-runner{width:12%;font-family:var(--family-mono)}.activity-table .event-agent{width:11%}.activity-table .event-action{width:21%;font-weight:500}.activity-table .event-tool{width:13%;color:var(--fg-muted)}.activity-table .event-outcome{width:18%}.activity-table .event-ms{width:6%;text-align:right;font-family:var(--family-mono);color:var(--fg-muted)}.runner-chip{display:inline-block;padding:1px 7px;border:1px solid var(--border);border-radius:999px;background:var(--bg);font-size:var(--font-xs);max-width:100%;overflow:hidden;text-overflow:ellipsis;vertical-align:middle}.event-outcome .dot{margin-right:.45em}.event-filters{margin-bottom:1em}.event-filters select{width:auto;min-width:11em;padding:4px 8px}
@media(max-width:850px){.fleet-hero{grid-template-columns:1fr;gap:1.2em}}@media(max-width:700px){.fields{grid-template-columns:1fr}.nav-wrap{align-items:flex-start;flex-wrap:wrap}.nav-status{width:100%;margin-left:0}.nav-tabs{order:3;width:100%;overflow-x:auto;margin-left:0}.fleet-metrics{grid-template-columns:repeat(3,1fr)}.fleet-metric{padding:.2em .65em}.activity-table .event-ms,.activity-table th.event-ms{display:none}}")

(def ^:private fleet-tabs
  [[:overview "overview" "/"]
   [:agents "agents" "/agents"]
   [:events "events" "/events"]
   [:usage "usage" "/usage"]])

(defn- page [title active identity & content]
  (str "<!doctype html>"
       (hic/html
         [:html {:lang "en"}
          [:head
           [:meta {:charset "utf-8"}]
           [:meta {:name "viewport" :content "width=device-width, initial-scale=1"}]
           [:title (str "cch · " title)]
           [:style (hic/raw (web/base-css))]
           [:style (hic/raw hosted-css)]]
          [:body
           [:div.page-wrap.hosted-page
            (web/nav-bar
              {:active active
               :tabs fleet-tabs
               :status "control plane · online"
               :actions
               (when identity
                 [:form.hosted-signout {:method "post" :action "/logout"}
                  [:input {:type "hidden" :name "csrf" :value (:csrf identity)}]
                  [:button.btn {:type "submit"} "sign out"]])})
            [:div.page-header
             [:div
              [:h1 title]
              [:p.hosted-subtitle "Local execution · global coordination"]]]
            content]]])))

(defn- primary-name [session]
  (or (:alias session) (:name session) (:mnemonic session)))

(defn- visible-name [session duplicate-names]
  (let [primary (primary-name session)]
    (if (and (contains? duplicate-names primary)
             (not= primary (:mnemonic session)))
      (str primary " · " (:mnemonic session))
      primary)))

(defn- runner-label [runner-id]
  (when runner-id
    (->> (str/split (str/replace-first runner-id #"^runner-" "") #"[-_]+")
         (map str/capitalize)
         (str/join " "))))

(defn- session-card [session csrf duplicate-names]
  (let [{:keys [key label]} (attention session)]
    [:article.session
     [:div.session-top
      [:strong (visible-name session duplicate-names)]
      [:span.badge {:class (str "badge-" key)} label]]
     [:div.session-context
      [:span (str/capitalize (:agent session))]
      [:span.context-separator "·"]
      [:span.runner-chip {:title (:runner-id session)}
       (runner-label (:runner-id session))]]
     (when-let [native-url (:native-url session)]
       [:div.provider
        [:a {:href native-url :target "_blank" :rel "noopener noreferrer"}
         (str "Open this " (str/capitalize (:agent session)) " session")]])
     [:details.session-details
      [:summary "Details & rename"]
      [:div.session-meta
       (when-let [native-name (:name session)]
         [:div [:small "Provider name"] [:div native-name]])
       [:div [:small "Mnemonic"] [:div.route (:mnemonic session)]]
       [:div [:small "Route"] [:div.route (:id session)]]
       [:div [:small "Runner"] [:div.route (:runner-id session)]]
       [:div [:small "Native state"] [:div.route (:status session)]]]
      [:form.alias-form {:method "post" :action "/sessions/alias"}
       [:input {:type "hidden" :name "csrf" :value csrf}]
       [:input {:type "hidden" :name "target" :value (:id session)}]
       [:label {:for (str "alias-" (:id session))} "Alias"]
       [:div.actions
        [:input {:id (str "alias-" (:id session))
                 :name "alias" :value (or (:alias session) "")
                 :maxlength naming/max-alias-length
                 :placeholder "Optional broker-visible name"}]
        [:button.secondary {:type "submit"} "Save"]]
       [:small "Visible to paired participants; avoid secrets or private paths."]]]]))

(defn- switchboard-page [b identity {:keys [error message-id]}]
  (let [sessions (broker/active-sessions b)
        name-counts (frequencies (map primary-name sessions))
        duplicate-names (->> name-counts
                             (keep (fn [[session-name count]]
                                     (when (< 1 count) session-name)))
                             set)
        runners (count (set (map :runner-id sessions)))
        message (when message-id
                  (broker/operator-message-metadata b message-id))]
    (page
      "agents" :agents identity
      [:section.summary.panel
       [:div [:strong (count sessions)] [:small "active sessions"]]
       [:div [:strong runners] [:small "connected runners"]]
       [:div [:strong (count (filter #(= "attention" (:key (attention %))) sessions))]
        [:small "need attention"]]]
      (when error [:div.notice.error error])
      (when message
        [:div.notice {:class (str "status-" (:status message))}
         [:strong "Message " (:status message)]
         [:div.route (:message-id message)]
         [:small "To " (:target message) " · attempts " (:attempts message)]
         (when-not (contains? #{"delivered" "failed" "expired"}
                              (:status message))
           [:div [:a {:href (str "/?message-id="
                                 (url-encode (:message-id message)))}
                  "Refresh delivery status"]])])
      [:section.panel
       [:h2 "Sessions"]
       [:p.muted "Choose a session by name. Routing details and rename controls are collapsed."]
       (if (seq sessions)
         [:div.sessions
          (map #(session-card % (:csrf identity) duplicate-names) sessions)]
         [:div.empty "No active runner leases are currently visible."])]
      [:section.panel
       [:h2 "Send ordinary text"]
       [:p.muted
        "Messages arrive as normal user text. Commands, approvals, terminal input, and credentials are not accepted."]
       (if (seq sessions)
         [:form {:method "post" :action "/messages"}
          [:input {:type "hidden" :name "csrf" :value (:csrf identity)}]
          [:div.fields
           [:div
            [:label {:for "target"} "Target session"]
            [:select {:id "target" :name "target" :required true}
             (for [target sessions]
               [:option {:value (:id target)}
                (str (visible-name target duplicate-names) " · "
                     (str/capitalize (:agent target)) " · "
                     (runner-label (:runner-id target)) " · "
                     (:status target))])]]
           [:div
            [:label {:for "message"} "Message"]
            [:textarea {:id "message" :name "message" :required true
                        :maxlength 8000
                        :placeholder "Ask this agent to inspect, review, or continue…"}]]]
          [:div.actions [:button {:type "submit"} "Send message"]]]
         [:p.muted "A session must be active before a message can be routed."])]
      )))

(declare agent-label)

(def ^:private activity-time-format
  (.withZone (DateTimeFormatter/ofPattern "MMM d HH:mm:ss")
             (ZoneId/systemDefault)))

(defn- activity-time [observed-at]
  (.format activity-time-format (Instant/ofEpochMilli observed-at)))

(defn- activity-dot [outcome]
  [:span.dot
   {:class (case outcome
             ("allowed" "completed") "dot-allow"
             "approval-needed" "dot-ask"
             ("denied" "failed") "dot-deny"
             "dot-observe")}])

(defn- activity-table [observations]
  (if (seq observations)
    [:table.dense-table.activity-table
     [:thead
      [:tr
       [:th.event-time "Time"]
       [:th.event-runner "Runner"]
       [:th.event-agent "Agent"]
       [:th.event-action "Activity"]
       [:th.event-tool "Tool"]
       [:th.event-outcome "Outcome"]
       [:th.event-ms "ms"]]]
     [:tbody
      (for [{:keys [event-id observed-at runner-id agent action tool-category outcome
                    duration-ms]} observations]
        [:tr {:data-event event-id}
         [:td.event-time (activity-time observed-at)]
         [:td.event-runner
          (if runner-id
            [:span.runner-chip {:title runner-id} (runner-label runner-id)]
            "—")]
         [:td.event-agent (agent-label agent)]
         [:td.event-action (str/replace action "." " · ")]
         [:td.event-tool (or tool-category "—")]
         [:td.event-outcome (activity-dot outcome) outcome]
         [:td.event-ms (when (number? duration-ms)
                         (if (< duration-ms 1.0)
                           "<1"
                           (str (Math/round (double duration-ms)))))]])]]
    [:div.empty "No normalized agent activity is available yet."]))

(defn- parse-activity-query [query runner-ids]
  (let [agent (some-> (:agent query) str/lower-case not-empty)
        action (some-> (:action query) str/lower-case not-empty)
        runner-id (some-> (:runner query) not-empty)]
    (cond-> {:limit 200}
      (contains? #{"claude-code" "codex" "agy"} agent) (assoc :agent agent)
      (contains? #{"session.started" "session.stopped" "session.ended"
                   "turn.started" "tool.requested" "tool.completed"
                   "tool.failed" "tool.permission" "context.compacted"
                   "attention.requested"} action)
      (assoc :action action)
      (contains? runner-ids runner-id) (assoc :runner-id runner-id))))

(defn- events-page [b identity query]
  (let [runners (sort-by :runner-id (broker/active-runners b))
        runner-ids (set (map :runner-id runners))
        {:keys [agent action runner-id] :as filters}
        (parse-activity-query query runner-ids)
        observations (broker/recent-activity-observations b filters)]
    (page
      "events" :events identity
      [:p.page-subtitle
       "Activity across Claude, Codex, and AGY"]
      [:form.filter-bar.event-filters {:method "get" :action "/events"}
       [:label {:for "event-runner"} "Runner"]
       [:select {:id "event-runner" :name "runner"}
        [:option {:value ""} "All runners"]
        (for [{value :runner-id} runners]
          [:option (cond-> {:value value}
                     (= value runner-id) (assoc :selected true))
           (runner-label value)])]
       [:label {:for "event-agent"} "Agent"]
       [:select {:id "event-agent" :name "agent"}
        [:option {:value ""} "All agents"]
        (for [[value label] [["claude-code" "Claude"] ["codex" "Codex"]
                             ["agy" "AGY"]]]
          [:option (cond-> {:value value} (= value agent) (assoc :selected true))
           label])]
       [:label {:for "event-action"} "Activity"]
       [:select {:id "event-action" :name "action"}
        [:option {:value ""} "All activity"]
        (for [value ["session.started" "session.stopped" "session.ended"
                     "turn.started" "tool.requested" "tool.completed"
                     "tool.failed" "tool.permission" "context.compacted"
                     "attention.requested"]]
          [:option (cond-> {:value value} (= value action) (assoc :selected true))
           (str/replace value "." " · ")])]
       [:button.btn {:type "submit"} "filter"]
       [:a.btn {:href "/events"} "clear"]]
      [:section.panel (activity-table observations)])))

(defn- overview-page [b identity]
  (let [sessions (broker/active-sessions b)
        runners (count (set (map :runner-id sessions)))
        needs-attention (count (filter #(= "attention" (:key (attention %)))
                                       sessions))
        recent (broker/recent-activity-observations b {:limit 8})]
    (page
      "overview" :overview identity
      [:section.panel.fleet-hero
       [:div.fleet-state
        [:div.fleet-kicker [:span.dot.dot-allow] "Fleet status"]
        [:h2.fleet-title
         (if (zero? needs-attention)
           "Everything is ready"
           (str needs-attention " agent" (when (not= 1 needs-attention) "s")
                " need attention"))]
        [:p.fleet-copy "Execution stays local while presence, activity, and usage meet here."]]
       [:div.fleet-metrics
        [:div.fleet-metric [:strong (count sessions)] [:small "active agents"]]
        [:div.fleet-metric [:strong runners] [:small "runners"]]
        [:div.fleet-metric [:strong needs-attention] [:small "need attention"]]]]
      [:section.panel.recent-panel
       [:div.recent-heading
        [:div [:h2.inline "Recent activity"]
         [:span.live-label [:span.dot] "live"]]
        [:a {:href "/events"} "View all"]]
       (activity-table recent)])))

(defn- agent-label [agent]
  (case agent
    "claude-code" "Claude Code"
    "agy" "AGY"
    (str/capitalize agent)))

(defn- cached-usage-forecast [b cache]
  (let [now (System/currentTimeMillis)
        cached @cache]
    (if (< (- now (:cached-at cached 0)) 30000)
      (:forecast cached)
      (let [forecast (-> (broker/usage-forecast-inputs b)
                         usage-forecast/from-read-model)]
        (reset! cache {:cached-at now :forecast forecast})
        forecast))))

(defn- parse-usage-selection [query]
  {:window (case (some-> (:window query) str/lower-case)
             ("5h" "5hour" "5-hour" "five-hour" "fivehour") :five-hour
             :seven-day)
   :agent (case (some-> (:agent query) str/lower-case)
            "codex" "codex"
            ("agy" "antigravity") "agy"
            ("claude" "claude-code" "cc") "claude-code"
            "claude-code")})

(defn- usage-page [b identity cache query]
  (let [forecast (cached-usage-forecast b cache)
        {:keys [window agent]} (parse-usage-selection query)
        window-name (if (= window :five-hour) "five_hour" "seven_day")
        data (some-> (get-in forecast [:agents agent window-name :page-data])
                     (assoc :agent agent))
        subtitle (if (= window :five-hour)
                   "5-hour rate-limit window · fleet-wide projection with 90% credible interval"
                   "7-day rate-limit window · fleet-wide projection with 90% credible interval")]
    (page
      "usage" :usage identity
      [:p.page-subtitle subtitle]
      (usage/page-view data {:base "/usage" :window window :agent agent}))))

(defn- error-page [status title message]
  (response status (page title nil nil [:section.panel [:h2 title] [:p message]])))

(defn handler
  "Build a Ring handler for human routes only. Every request must carry a
  valid Cloudflare Access assertion; runner credentials are never accepted."
  [b config]
  (let [usage-cache (atom nil)]
    (fn [{:keys [request-method uri] :as request}]
      (when (contains? #{"/" "/agents" "/events" "/usage" "/messages" "/sessions/alias" "/logout"} uri)
        (try
        (let [identity (auth/authenticate! config request)
              identity (assoc identity :csrf (auth/csrf-token config identity))]
          (cond
            (and (= :get request-method) (= "/" uri))
            (response 200 (overview-page b identity))

            (and (= :get request-method) (= "/agents" uri))
            (response 200 (switchboard-page
                            b identity {:message-id (:message-id
                                                     (request-query request))}))

            (and (= :get request-method) (= "/usage" uri))
            (response 200 (usage-page b identity usage-cache
                                      (request-query request)))

            (and (= :get request-method) (= "/events" uri))
            (response 200 (events-page b identity (request-query request)))

            (and (= :post request-method) (= "/messages" uri))
            (let [form (request-form request)]
              (csrf! config request identity form)
              (let [result (broker/enqueue-operator-message!
                             b {:target (:target form)
                                :message (:message form)
                                :message-id (str (random-uuid))})]
                (redirect (str "/agents?message-id="
                               (url-encode (:message-id result))))))

            (and (= :post request-method) (= "/sessions/alias" uri))
            (let [form (request-form request)]
              (csrf! config request identity form)
              (broker/set-operator-session-alias!
                b {:route-id (:target form) :alias (:alias form)})
              (redirect "/agents"))

            (and (= :post request-method) (= "/logout" uri))
            (let [form (request-form request)]
              (csrf! config request identity form)
              (redirect (auth/logout-location config)))

            :else
            (response 405 "Method not allowed")))
        (catch clojure.lang.ExceptionInfo error
          (case (:type (ex-data error))
            :identity-not-allowed
            (error-page 403 "Account not allowed"
                        "This Access identity is not authorized for this switchboard.")

            (:access-required :invalid-access-token :access-key-unavailable
             :access-upstream-error)
            (error-page 401 "Access required"
                        "A valid Cloudflare Access session is required.")

            (:invalid-csrf :invalid-form)
            (do
              (log-form-rejection! error)
              (error-page
                403 "Request rejected"
                "The form expired or did not originate from this switchboard."))

            (:unknown-session :invalid-alias :invalid-message :message-too-large
             :command-mode-not-allowed)
            (let [identity (auth/authenticate! config request)
                  identity (assoc identity :csrf
                                  (auth/csrf-token config identity))]
              (response 422 (switchboard-page b identity
                                              {:error (.getMessage error)})))

            (error-page 400 "Request failed" "The request could not be completed.")))
          (catch Exception _
            (error-page 500 "Switchboard unavailable"
                        "The switchboard could not complete this request.")))))))
