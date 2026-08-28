(ns cch.control.switchboard
  "Server-rendered, Google-protected human switchboard for sanitized broker
  presence and ordinary-text routing. Provider-native UIs remain the only
  surfaces for transcripts, terminal control, and approvals."
  (:require [cch.control.broker-api :as broker]
            [cch.control.web-auth :as auth]
            [clojure.string :as str]
            [hiccup2.core :as hic])
  (:import [java.net URLDecoder URLEncoder]
           [java.nio.charset StandardCharsets]))

(def ^:private provider-links
  {"claude" {:href "https://claude.ai/code" :label "Open Claude Code"}
   "codex" {:href "https://chatgpt.com/codex" :label "Open Codex"}})

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

(defn- request-cookies [request]
  (->> (str/split (or (get-in request [:headers "cookie"]) "") #";")
       (keep (fn [part]
               (let [[name value] (str/split (str/trim part) #"=" 2)]
                 (when-not (str/blank? name) [name (or value "")]))))
       (into {})))

(defn- browser-session [config request]
  (auth/session config
                (get (request-cookies request) auth/session-cookie-name)))

(defn- csrf! [config request session form]
  (when-not (= (:origin config) (get-in request [:headers "origin"]))
    (throw (ex-info "Request origin is invalid" {:type :invalid-csrf})))
  (when-not (auth/csrf-valid? session (:csrf form))
    (throw (ex-info "CSRF token is invalid" {:type :invalid-csrf})))
  true)

(defn- attention [session]
  (case (str/lower-case (or (:status session) "unknown"))
    ("waiting" "blocked" "needs-attention")
    {:key "attention" :label "Needs you"}

    ("working" "running" "active")
    {:key "working" :label "Working"}

    {:key "ready" :label "Ready"}))

(defn- page [title session & content]
  (str "<!doctype html>"
       (hic/html
         [:html {:lang "en"}
          [:head
           [:meta {:charset "utf-8"}]
           [:meta {:name "viewport" :content "width=device-width, initial-scale=1"}]
           [:title (str title " · cch control")]
           [:style
            "
:root{color-scheme:light dark;--bg:#0d1117;--panel:#161b22;--line:#30363d;--text:#e6edf3;--muted:#8b949e;--blue:#58a6ff;--green:#3fb950;--amber:#d29922;--red:#f85149}*{box-sizing:border-box}body{margin:0;background:var(--bg);color:var(--text);font:15px/1.45 ui-sans-serif,system-ui,-apple-system,sans-serif}main{width:min(1100px,calc(100% - 28px));margin:0 auto;padding:28px 0 60px}header{display:flex;gap:18px;align-items:center;justify-content:space-between;margin-bottom:24px}h1{font-size:24px;margin:0}h2{font-size:17px;margin:0 0 14px}.muted,small{color:var(--muted)}a{color:var(--blue)}.panel{background:var(--panel);border:1px solid var(--line);border-radius:12px;padding:18px;margin:0 0 18px}.summary{display:flex;gap:20px;flex-wrap:wrap}.summary strong{font-size:22px;display:block}.sessions{display:grid;grid-template-columns:repeat(auto-fit,minmax(270px,1fr));gap:12px}.session{border:1px solid var(--line);border-radius:10px;padding:14px;min-width:0}.session-top{display:flex;justify-content:space-between;gap:10px;align-items:center}.route{font:12px/1.4 ui-monospace,SFMono-Regular,monospace;word-break:break-all;color:var(--muted)}.badge{border-radius:999px;padding:3px 9px;font-size:12px;border:1px solid var(--line)}.badge-attention{color:#fff;background:#7a4b00;border-color:var(--amber)}.badge-working{color:#fff;background:#174f2a;border-color:var(--green)}.badge-ready{color:#fff;background:#17365d;border-color:var(--blue)}label{display:block;margin:0 0 6px;color:var(--muted)}select,textarea{width:100%;border:1px solid var(--line);border-radius:8px;background:var(--bg);color:var(--text);padding:10px;font:inherit}textarea{min-height:120px;resize:vertical}.fields{display:grid;grid-template-columns:1fr 2fr;gap:14px}@media(max-width:700px){.fields{grid-template-columns:1fr}header{align-items:flex-start}}button,.button{display:inline-block;border:1px solid #388bfd;border-radius:8px;background:#238636;color:white;padding:9px 14px;font:inherit;text-decoration:none;cursor:pointer}button.secondary{background:transparent;border-color:var(--line)}.actions{display:flex;gap:10px;align-items:center;margin-top:12px}.notice{border-left:3px solid var(--blue);padding:10px 12px;background:var(--panel);margin-bottom:18px}.error{border-color:var(--red);color:#ffb3ad}.status-delivered{border-color:var(--green)}.status-failed,.status-expired{border-color:var(--red)}code{font-family:ui-monospace,SFMono-Regular,monospace}.inline{display:inline}.provider{margin-top:10px}.empty{text-align:center;padding:28px;color:var(--muted)}"]]
          [:body
           [:main
            [:header
             [:div [:h1 "Multi-agent control"]
              [:small "Local execution · brokered presence and text routing"]]
             (when session
               [:form.inline {:method "post" :action "/logout"}
                [:input {:type "hidden" :name "csrf" :value (:csrf session)}]
                [:button.secondary {:type "submit"} "Sign out"]])]
            content]]])))

(defn- session-card [session]
  (let [{:keys [key label]} (attention session)
        provider (get provider-links (:agent session))]
    [:article.session
     [:div.session-top
      [:strong (str/capitalize (:agent session))]
      [:span.badge {:class (str "badge-" key)} label]]
     [:div.route (:id session)]
     [:small "Runner: " (:runner-id session) " · Native state: " (:status session)]
     (when provider
       [:div.provider
        [:a {:href (:href provider) :target "_blank" :rel "noopener noreferrer"}
         (:label provider)]])]))

(defn- switchboard-page [b session {:keys [error message-id]}]
  (let [sessions (broker/active-sessions b)
        runners (count (set (map :runner-id sessions)))
        message (when message-id
                  (broker/operator-message-metadata b message-id))]
    (page
      "Switchboard" session
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
       (if (seq sessions)
         [:div.sessions (map session-card sessions)]
         [:div.empty "No active runner leases are currently visible."])]
      [:section.panel
       [:h2 "Send ordinary text"]
       [:p.muted
        "Messages arrive as normal user text. Commands, approvals, terminal input, and credentials are not accepted."]
       (if (seq sessions)
         [:form {:method "post" :action "/messages"}
          [:input {:type "hidden" :name "csrf" :value (:csrf session)}]
          [:div.fields
           [:div
            [:label {:for "target"} "Target session"]
            [:select {:id "target" :name "target" :required true}
             (for [target sessions]
               [:option {:value (:id target)}
                (str (str/capitalize (:agent target)) " · " (:status target)
                     " · " (:runner-id target))])]]
           [:div
            [:label {:for "message"} "Message"]
            [:textarea {:id "message" :name "message" :required true
                        :maxlength 8000
                        :placeholder "Ask this agent to inspect, review, or continue…"}]]]
          [:div.actions [:button {:type "submit"} "Send message"]]]
         [:p.muted "A session must be active before a message can be routed."])]
      [:section.panel
       [:h2 "Native authority"]
       [:p.muted
        "Open the provider-native interface for transcripts, approvals, terminal control, and rich session history. This switchboard intentionally does not copy them."]])))

(defn- error-page [status title message]
  (response status (page title nil [:section.panel [:h2 title] [:p message]
                                    [:a.button {:href "/login"} "Try another account"]])))

(defn handler
  "Build a Ring handler for human routes only. Returns nil for runner API and
  health routes so the broker boundary can handle them independently."
  [b config]
  (fn [{:keys [request-method uri] :as request}]
    (when (contains? #{"/" "/login" "/auth/google/callback"
                       "/messages" "/logout"} uri)
      (try
        (let [session (browser-session config request)]
          (cond
            (and (= :get request-method) (= "/login" uri))
            (if session
              (redirect "/")
              (let [{:keys [location transaction]} (auth/begin-login config)]
                (redirect location
                          (auth/cookie auth/transaction-cookie-name transaction
                                       (quot auth/login-ttl-ms 1000)))))

            (and (= :get request-method) (= "/auth/google/callback" uri))
            (let [query (request-query request)]
              (when (:error query)
                (throw (ex-info "Google sign-in was not completed"
                                {:type :invalid-login-state})))
              (let [identity (auth/complete-login!
                               config {:code (:code query)
                                       :state (:state query)
                                       :transaction
                                       (get (request-cookies request)
                                            auth/transaction-cookie-name)})
                    token (auth/new-session config identity)]
                (redirect "/"
                          [(auth/cookie auth/session-cookie-name token
                                        (quot (:session-ttl-ms config) 1000))
                           (auth/cookie auth/transaction-cookie-name)])))

            (and (= :get request-method) (= "/" uri))
            (if session
              (response 200 (switchboard-page
                              b session {:message-id (:message-id
                                                      (request-query request))}))
              (redirect "/login"))

            (and (= :post request-method) (= "/messages" uri))
            (if-not session
              (redirect "/login")
              (let [form (request-form request)]
                (csrf! config request session form)
                (let [result (broker/enqueue-operator-message!
                               b {:target (:target form)
                                  :message (:message form)
                                  :message-id (str (random-uuid))})]
                  (redirect (str "/?message-id="
                                 (url-encode (:message-id result)))))))

            (and (= :post request-method) (= "/logout" uri))
            (if-not session
              (redirect "/login")
              (let [form (request-form request)]
                (csrf! config request session form)
                (redirect "/login" (auth/cookie auth/session-cookie-name))))

            :else
            (response 405 "Method not allowed")))
        (catch clojure.lang.ExceptionInfo error
          (case (:type (ex-data error))
            :identity-not-allowed
            (error-page 403 "Account not allowed"
                        "This Google account is not authorized for this switchboard.")

            (:invalid-login-state :invalid-id-token :oidc-upstream-error)
            (error-page 400 "Sign-in failed"
                        "The Google sign-in could not be verified. Please try again.")

            (:invalid-csrf :invalid-form)
            (error-page 403 "Request rejected"
                        "The form expired or did not originate from this switchboard.")

            (:unknown-session :invalid-message :message-too-large
             :command-mode-not-allowed)
            (if-let [session (browser-session config request)]
              (response 422 (switchboard-page b session
                                              {:error (.getMessage error)}))
              (redirect "/login"))

            (error-page 400 "Request failed" "The request could not be completed.")))
        (catch Exception _
          (error-page 500 "Switchboard unavailable"
                      "The switchboard could not complete this request."))))))
