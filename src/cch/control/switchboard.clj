(ns cch.control.switchboard
  "Server-rendered, Cloudflare Access-protected switchboard for sanitized broker
  presence and ordinary-text routing. Provider-native UIs remain the only
  surfaces for transcripts, terminal control, and approvals."
  (:require [cch.control.broker-api :as broker]
            [cch.control.naming :as naming]
            [cch.control.usage-forecast :as usage-forecast]
            [cch.control.web-auth :as auth]
            [clojure.string :as str]
            [hiccup2.core :as hic])
  (:import [java.net URI URLDecoder URLEncoder]
           [java.nio.charset StandardCharsets]))

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

(defn- page [title identity & content]
  (str "<!doctype html>"
       (hic/html
         [:html {:lang "en"}
          [:head
           [:meta {:charset "utf-8"}]
           [:meta {:name "viewport" :content "width=device-width, initial-scale=1"}]
           [:title (str title " · cch control")]
           [:style
            "
:root{color-scheme:light dark;--bg:#0d1117;--panel:#161b22;--line:#30363d;--text:#e6edf3;--muted:#8b949e;--blue:#58a6ff;--green:#3fb950;--amber:#d29922;--red:#f85149}*{box-sizing:border-box}body{margin:0;background:var(--bg);color:var(--text);font:15px/1.45 ui-sans-serif,system-ui,-apple-system,sans-serif}main{width:min(1100px,calc(100% - 28px));margin:0 auto;padding:28px 0 60px}header{display:flex;gap:18px;align-items:center;justify-content:space-between;margin-bottom:24px}h1{font-size:24px;margin:0}h2{font-size:17px;margin:0 0 14px}.muted,small{color:var(--muted)}a{color:var(--blue)}.header-actions,.appnav,.local-links{display:flex;gap:10px;align-items:center;flex-wrap:wrap}.appnav a{padding:7px 10px;border-radius:7px;text-decoration:none;border:1px solid var(--line)}.panel{background:var(--panel);border:1px solid var(--line);border-radius:12px;padding:18px;margin:0 0 18px}.summary{display:flex;gap:20px;flex-wrap:wrap}.summary strong{font-size:22px;display:block}.sessions,.forecast-grid{display:grid;grid-template-columns:repeat(auto-fit,minmax(270px,1fr));gap:12px}.session,.forecast-card{border:1px solid var(--line);border-radius:10px;padding:14px;min-width:0}.forecast-value{font-size:28px;font-weight:700}.forecast-meta{display:grid;grid-template-columns:1fr 1fr;gap:10px;margin-top:12px}.session-top{display:flex;justify-content:space-between;gap:10px;align-items:center}.route{font:12px/1.4 ui-monospace,SFMono-Regular,monospace;word-break:break-all;color:var(--muted)}.badge{border-radius:999px;padding:3px 9px;font-size:12px;border:1px solid var(--line)}.badge-attention{color:#fff;background:#7a4b00;border-color:var(--amber)}.badge-working{color:#fff;background:#174f2a;border-color:var(--green)}.badge-ready{color:#fff;background:#17365d;border-color:var(--blue)}label{display:block;margin:0 0 6px;color:var(--muted)}select,textarea,.alias-form input[name=alias]{width:100%;border:1px solid var(--line);border-radius:8px;background:var(--bg);color:var(--text);padding:10px;font:inherit}.alias-form{margin-top:12px}.alias-form .actions{margin-top:0}.alias-form input[name=alias]{min-width:0;flex:1}textarea{min-height:120px;resize:vertical}.fields{display:grid;grid-template-columns:1fr 2fr;gap:14px}.session-details{border-top:1px solid var(--line);margin-top:12px;padding-top:9px}.session-details summary{color:var(--muted);cursor:pointer;font-size:13px}.session-meta{display:grid;gap:7px;margin-top:10px}.session-meta small{display:block}.session-meta div:not(.route){min-width:0}@media(max-width:700px){.fields{grid-template-columns:1fr}.header-actions{justify-content:flex-end}header{align-items:flex-start}.forecast-meta{grid-template-columns:1fr}}button,.button{display:inline-block;border:1px solid #388bfd;border-radius:8px;background:#238636;color:white;padding:9px 14px;font:inherit;text-decoration:none;cursor:pointer}button.secondary{background:transparent;border-color:var(--line)}.actions{display:flex;gap:10px;align-items:center;margin-top:12px}.notice{border-left:3px solid var(--blue);padding:10px 12px;background:var(--panel);margin-bottom:18px}.error{border-color:var(--red);color:#ffb3ad}.status-delivered{border-color:var(--green)}.status-failed,.status-expired{border-color:var(--red)}code{font-family:ui-monospace,SFMono-Regular,monospace}.inline{display:inline}.provider{margin-top:10px}.empty{text-align:center;padding:28px;color:var(--muted)}"]]
          [:body
           [:main
            [:header
             [:div [:h1 "Multi-agent control"]
              [:small "Local execution · brokered presence and text routing"]]
             (when identity
               [:div.header-actions
                [:nav.appnav
                 [:a {:href "/"} "Agents"]
                 [:a {:href "/usage"} "Usage"]]
                [:form.inline {:method "post" :action "/logout"}
                 [:input {:type "hidden" :name "csrf" :value (:csrf identity)}]
                 [:button.secondary {:type "submit"} "Sign out"]]])]
            content]]])))

(defn- primary-name [session]
  (or (:alias session) (:name session) (:mnemonic session)))

(defn- visible-name [session duplicate-names]
  (let [primary (primary-name session)]
    (if (and (contains? duplicate-names primary)
             (not= primary (:mnemonic session)))
      (str primary " · " (:mnemonic session))
      primary)))

(defn- session-card [session csrf duplicate-names]
  (let [{:keys [key label]} (attention session)]
    [:article.session
     [:div.session-top
      [:strong (visible-name session duplicate-names)]
      [:span.badge {:class (str "badge-" key)} label]]
     [:small (str/capitalize (:agent session))]
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

(declare local-tools)

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
      "Switchboard" identity
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
                     (:status target))])]]
           [:div
            [:label {:for "message"} "Message"]
            [:textarea {:id "message" :name "message" :required true
                        :maxlength 8000
                        :placeholder "Ask this agent to inspect, review, or continue…"}]]]
          [:div.actions [:button {:type "submit"} "Send message"]]]
         [:p.muted "A session must be active before a message can be routed."])]
      (local-tools b))))

(defn- agent-label [agent]
  (case agent "claude-code" "Claude Code" (str/capitalize agent)))

(defn- duration-label [seconds]
  (let [seconds (max 0 (long seconds))
        days (quot seconds 86400)
        hours (quot (mod seconds 86400) 3600)
        minutes (quot (mod seconds 3600) 60)]
    (cond
      (pos? days) (format "%dd %dh" days hours)
      (pos? hours) (format "%dh %dm" hours minutes)
      :else (format "%dm" minutes))))

(defn- local-tools [b]
  (let [runners (filter :local-ui-url (broker/active-runners b))]
    [:section.panel
     [:h2 "Local tools"]
     [:p.muted "Hooks, raw events, configuration, and debugging stay on each runner and open directly; this application does not proxy them."]
     (if (seq runners)
       (for [{:keys [runner-id local-ui-url]} runners]
         [:div.local-links
          [:strong runner-id]
          [:a {:href local-ui-url :target "_blank" :rel "noopener noreferrer"} "Overview"]
          [:a {:href (str local-ui-url "/hooks") :target "_blank" :rel "noopener noreferrer"} "Hooks"]
          [:a {:href (str local-ui-url "/events") :target "_blank" :rel "noopener noreferrer"} "Events"]
          [:a {:href (str local-ui-url "/debug") :target "_blank" :rel "noopener noreferrer"} "Debug"]])
       [:p.muted "No runner has advertised a reachable local UI URL."])]))

(defn- usage-page [b identity]
  (let [forecast (-> (broker/usage-forecast-inputs b)
                     usage-forecast/from-read-model)
        agents (:agents forecast)]
    (page
      "Usage" identity
      [:section.panel
       [:h2 "Usage & forecast"]
       [:p.muted "Fleet-wide projections from normalized usage observations. No account, session, machine, repository, transcript, or raw provider payload is stored in this view."]
       (if (some seq (vals agents))
         [:div.forecast-grid
          (for [[agent windows] agents
                [window {:keys [current-pct projected-pct seconds-left
                                sample-count band]}] windows]
            [:article.forecast-card
             [:div.session-top
              [:strong (agent-label agent)]
              [:span.badge (if (= window "five_hour") "5 hour" "7 day")]]
             [:div.forecast-value (str current-pct "%")]
             [:small (str "Projected " projected-pct "%")]
             [:div.forecast-meta
              [:div [:small "Resets in"] [:div (duration-label seconds-left)]]
              [:div [:small "Samples"] [:div sample-count]]
              (when band
                [:div [:small "Projection band"]
                 [:div (str (:lo band) "%–" (:hi band) "%")]])]])]
         [:div.empty "No normalized usage observations are available yet."])]
      (local-tools b))))

(defn- error-page [status title message]
  (response status (page title nil [:section.panel [:h2 title] [:p message]])))

(defn handler
  "Build a Ring handler for human routes only. Every request must carry a
  valid Cloudflare Access assertion; runner credentials are never accepted."
  [b config]
  (fn [{:keys [request-method uri] :as request}]
    (when (contains? #{"/" "/usage" "/messages" "/sessions/alias" "/logout"} uri)
      (try
        (let [identity (auth/authenticate! config request)
              identity (assoc identity :csrf (auth/csrf-token config identity))]
          (cond
            (and (= :get request-method) (= "/" uri))
            (response 200 (switchboard-page
                            b identity {:message-id (:message-id
                                                     (request-query request))}))

            (and (= :get request-method) (= "/usage" uri))
            (response 200 (usage-page b identity))

            (and (= :post request-method) (= "/messages" uri))
            (let [form (request-form request)]
              (csrf! config request identity form)
              (let [result (broker/enqueue-operator-message!
                             b {:target (:target form)
                                :message (:message form)
                                :message-id (str (random-uuid))})]
                (redirect (str "/?message-id="
                               (url-encode (:message-id result))))))

            (and (= :post request-method) (= "/sessions/alias" uri))
            (let [form (request-form request)]
              (csrf! config request identity form)
              (broker/set-operator-session-alias!
                b {:route-id (:target form) :alias (:alias form)})
              (redirect "/"))

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
                      "The switchboard could not complete this request."))))))
