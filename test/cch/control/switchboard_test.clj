(ns cch.control.switchboard-test
  (:require [cch.control.broker :as broker]
            [cch.control.broker-http :as broker-http]
            [cch.control.naming :as naming]
            [cch.control.switchboard :as switchboard]
            [cch.control.web-auth :as auth]
            [cch.usage-observation :as usage]
            [clojure.string :as str]
            [clojure.test :refer [deftest is]])
  (:import [java.io ByteArrayInputStream StringWriter]
           [java.nio.charset StandardCharsets]))

(def config
  {:origin "https://control.invalid"
   :issuer "https://synthetic.cloudflareaccess.com"
   :audience "synthetic-access-audience"
   :allowed-emails #{"operator@example.invalid"}
   :session-secret "synthetic-session-secret-with-more-than-32-characters"
   :listen-host "127.0.0.1"
   :listen-port 8788
   :jwks-url "https://synthetic.cloudflareaccess.com/cdn-cgi/access/certs"})

(def access-identity
  {:subject "synthetic-access-subject"
   :email "operator@example.invalid"
   :issued-at 1000000
   :expires-at 2000000})

(def target "claude:00000000-0000-0000-0000-00000000000a")
(def assertion "synthetic-cloudflare-access-assertion")

(defn- registered-broker []
  (let [b (broker/new-broker {"runner-a" "synthetic-runner-token"
                              "runner-b" "synthetic-runner-token-b"})]
    (broker/register!
      b {:runner-id "runner-a"
         :token "synthetic-runner-token"
         :local-ui-url "https://runner.invalid/"
         :sessions [{:id target :agent "claude" :status "waiting"
                     :native-url "https://claude.ai/code/session_synthetic123"
                     :available true :cwd "/private/not-federated"
                     :name "Native <review>"}]})
    b))

(defn- body [value]
  (ByteArrayInputStream.
    (.getBytes ^String value StandardCharsets/UTF_8)))

(defn- access-request
  ([method uri]
   {:request-method method :uri uri
    :headers {"cf-access-jwt-assertion" assertion}})
  ([method uri extra]
   (merge (access-request method uri) extra)))

(defn- form-request [uri value]
  (access-request
    :post uri
    {:headers {"cf-access-jwt-assertion" assertion
               "origin" (:origin config)
               "content-type" "application/x-www-form-urlencoded"}
     :body (body value)}))

(defn- authenticated [f]
  (with-redefs [auth/authenticate!
                (fn [_ request]
                  (if (= assertion
                         (get-in request [:headers "cf-access-jwt-assertion"]))
                    access-identity
                    (throw (ex-info "missing assertion"
                                    {:type :access-required}))))]
    (f)))

(deftest human-pages-require-a-cloudflare-access-identity
  (let [handler (switchboard/handler (registered-broker) config)]
    (is (= 401 (:status (handler {:request-method :get :uri "/"}))))
    (is (= 401 (:status (handler {:request-method :get
                                  :uri "/favicon.svg"}))))
    (authenticated
      #(is (= 200 (:status (handler (access-request :get "/"))))))))

(deftest hosted-mark-and-favicon-share-the-meta-runner-treatment
  (authenticated
    (fn []
      (let [handler (switchboard/handler (registered-broker) config)
            page (:body (handler (access-request :get "/")))
            favicon (handler (access-request :get "/favicon.svg"))]
        (is (str/includes? page "href=\"/favicon.svg\""))
        (is (str/includes? page "fill=\"white\""))
        (is (str/includes? page "stroke=\"currentColor\""))
        (is (= 200 (:status favicon)))
        (is (= "image/svg+xml" (get-in favicon [:headers "Content-Type"])))
        (is (= "private, max-age=86400"
               (get-in favicon [:headers "Cache-Control"])))
        (is (str/includes? (get-in favicon [:headers "Content-Security-Policy"])
                           "img-src 'self'"))
        (is (str/includes? (:body favicon) "fill=\"white\""))
        (is (str/includes? (:body favicon) "stroke=\"#059669\""))
        (is (not (str/includes? (:body favicon) "synthetic")))))))

(deftest switchboard-renders-only-sanitized-presence-and-native-authority
  (authenticated
    (fn []
      (let [response ((switchboard/handler (registered-broker) config)
                      (access-request :get "/agents"))
            page (:body response)]
        (is (= 200 (:status response)))
        (is (str/includes? page target))
        (is (str/includes? page (naming/mnemonic target)))
        (is (str/includes? page "<strong>Native &lt;review&gt;</strong>"))
        (is (not (str/includes?
                   page
                   (str "<strong>Native &lt;review&gt; · "
                        (naming/mnemonic target) "</strong>"))))
        (is (not (str/includes? page "Native <review>")))
        (is (str/includes? page "Optional broker-visible name"))
        (is (str/includes? page "Details &amp; rename"))
        (is (str/includes? page "Routing details and rename controls are collapsed"))
        (is (str/includes? page "class=\"session-context\""))
        (is (str/includes? page "title=\"runner-a\">A</span>"))
        (is (str/includes? page "Native &lt;review&gt; · Claude · A · waiting"))
        (is (str/includes? page "Needs you"))
        (is (str/includes? page "Open this Claude session"))
        (is (str/includes? page "href=\"/usage\""))
        (is (not (str/includes? page "https://runner.invalid")))
        (is (not (str/includes? page ">Hooks<")))
        (is (str/includes? page
                           "https://claude.ai/code/session_synthetic123"))
        (is (not (str/includes? page "href=\"https://claude.ai/code\"")))
        (is (not (str/includes? page "Native authority")))
        (is (not (str/includes? page "/private/not-federated")))
        (is (not (str/includes? page assertion)))
        (is (= "no-store" (get-in response [:headers "Cache-Control"])))
        (is (str/includes? (get-in response [:headers "Content-Security-Policy"])
                           "frame-ancestors 'none'"))))))

(deftest authenticated-usage-page-renders-only-normalized-forecast-data
  (authenticated
    (fn []
      (let [b (registered-broker)
            now (System/currentTimeMillis)
            reset (quot (+ now 3600000) 1000)
            observations
            (mapv #(first
                     (usage/from-snapshot
                       {:agent "claude-code"
                        :observed-at (+ now %1)
                        :payload {:rate_limits
                                  {:five_hour {:used_percentage %2
                                               :resets_at reset}}}}))
                  [-60000 0] [12 14])]
        (broker/publish-usage!
          b {:runner-id "runner-a" :token "synthetic-runner-token"
             :observations observations})
        (let [response ((switchboard/handler b config)
                        (access-request :get "/usage"
                                        {:query-string "window=5h"}))
              page (:body response)]
          (is (= 200 (:status response)))
          (is (str/includes? page "quota used (%)"))
          (is (str/includes? page ">Claude<"))
          (is (str/includes? page "14%"))
          (is (str/includes? page "5-hour rate-limit window"))
          (is (not (str/includes? page "https://runner.invalid")))
          (is (not (str/includes? page target)))
          (is (= "no-store" (get-in response [:headers "Cache-Control"]))))))))

(deftest overview-and-events-render-normalized-global-activity
  (authenticated
    (fn []
      (let [b (registered-broker)
            event {:event-id (apply str (repeat 64 "d"))
                   :schema-version 1
                   :observed-at (System/currentTimeMillis)
                   :agent "codex"
                   :action "tool.completed"
                   :tool-category "write"
                   :outcome "completed"
                   :duration-ms 12.5}
            _ (broker/publish-activity!
                b {:runner-id "runner-a" :token "synthetic-runner-token"
                   :observations [event]})
            handler (switchboard/handler b config)
            overview (:body (handler (access-request :get "/")))
            events (:body (handler (access-request :get "/events")))]
        (is (str/includes? overview "Recent activity"))
        (is (str/includes? overview "agent need attention"))
        (is (str/includes? overview "Fleet status"))
        (is (str/includes? overview "tool · completed"))
        (doseq [redundant ["Coordinate" "Open agents" "Open events" "Open usage"]]
          (is (not (str/includes? overview redundant))))
        (is (str/includes? events "Activity across Claude, Codex, and AGY"))
        (is (str/includes? events "All runners"))
        (is (str/includes? events ">A</option>"))
        (is (str/includes? events "title=\"runner-a\""))
        (is (str/includes? events "Codex"))
        (is (str/includes? events "write"))
        (is (str/includes? events "completed"))
        (doseq [forbidden ["runner.invalid" "/private/not-federated"
                           "private-session" "private command"
                           "private prompt" "private reason"]]
          (is (not (str/includes? events forbidden))))))))

(deftest events-can-be-filtered-by-broker-authenticated-runner
  (authenticated
    (fn []
      (let [b (registered-broker)
            now (System/currentTimeMillis)
            event #(hash-map :event-id (apply str (repeat 64 %1))
                             :schema-version 1 :observed-at now
                             :agent "codex" :action "turn.started"
                             :outcome "observed")]
        (broker/register!
          b {:runner-id "runner-b" :token "synthetic-runner-token-b"
             :sessions []})
        (broker/publish-activity!
          b {:runner-id "runner-a" :token "synthetic-runner-token"
             :observations [(event "a")]})
        (broker/publish-activity!
          b {:runner-id "runner-b" :token "synthetic-runner-token-b"
             :observations [(event "b")]})
        (let [page (:body
                     ((switchboard/handler b config)
                      (access-request :get "/events"
                                      {:query-string "runner=runner-b"})))]
          (is (str/includes? page "selected=\"selected\" value=\"runner-b\""))
          (is (str/includes? page "data-event=\"bbbb"))
          (is (not (str/includes? page "data-event=\"aaaa"))))))))

(deftest duplicate-meaningful-names-show-mnemonics-for-disambiguation
  (authenticated
    (fn []
      (let [route-b "claude:00000000-0000-0000-0000-00000000000b"
            b (broker/new-broker {"runner-a" "synthetic-runner-token"})]
        (broker/register!
          b {:runner-id "runner-a"
             :token "synthetic-runner-token"
             :sessions [{:id target :agent "claude" :status "idle"
                         :available true :name "Review pair"}
                        {:id route-b :agent "claude" :status "idle"
                         :available true :name "Review pair"}]})
        (let [page (:body ((switchboard/handler b config)
                           (access-request :get "/agents")))]
          (is (str/includes?
                page
                (str "<strong>Review pair · "
                     (naming/mnemonic target) "</strong>")))
          (is (str/includes?
                page
                (str "<strong>Review pair · "
                     (naming/mnemonic route-b) "</strong>"))))))))

(deftest authenticated-operator-can-set-and-clear-a-bounded-alias
  (authenticated
    (fn []
      (let [b (registered-broker)
            handler (switchboard/handler b config)
            csrf (auth/csrf-token config access-identity)
            renamed (handler
                      (form-request
                        "/sessions/alias"
                        (str "csrf=" csrf "&target=" target
                             "&alias=Review+%3Cpair%3E")))]
        (is (= 303 (:status renamed)))
        (is (= "Review <pair>"
               (:alias (first (broker/sessions b)))))
        (let [page (:body (handler (access-request :get "/agents")))]
          (is (str/includes? page "Review &lt;pair&gt;"))
          (is (not (str/includes? page "Review <pair>"))))
        (is (= 422
               (:status
                 (handler
                   (form-request
                     "/sessions/alias"
                     (str "csrf=" csrf "&target=" target
                          "&alias=forged%0Aname"))))))
        (is (= 303
               (:status
                 (handler
                   (form-request
                     "/sessions/alias"
                     (str "csrf=" csrf "&target=" target "&alias="))))))
        (is (nil? (:alias (first (broker/sessions b)))))))))

(deftest valid-csrf-routes-transient-operator-text-and-reports-delivery
  (authenticated
    (fn []
      (let [b (registered-broker)
            handler (switchboard/handler b config)
            csrf (auth/csrf-token config access-identity)
            response (handler
                       (form-request
                         "/messages"
                         (str "csrf=" csrf "&target=" target
                              "&message=Synthetic+operator+request")))
            location (get-in response [:headers "Location"])
            message-id (second (re-find #"message-id=([^&]+)" location))
            delivered (-> (broker/poll! b {:runner-id "runner-a"
                                           :token "synthetic-runner-token"})
                          :messages first)]
        (is (= 303 (:status response)))
        (is (= "operator" (:source delivered)))
        (is (= "Synthetic operator request" (:body delivered)))
        (is (not (str/includes? (str response) "Synthetic operator request")))
        (broker/ack! b {:runner-id "runner-a"
                        :token "synthetic-runner-token"
                        :message-id (:message-id delivered)
                        :status "delivered"})
        (let [page (:body (handler
                            (access-request
                              :get "/agents"
                              {:query-string (str "message-id=" message-id)})))]
          (is (str/includes? page "Message delivered"))
          (is (not (str/includes? page "Synthetic operator request"))))))))

(deftest refreshed-access-assertion-does-not-expire-a-rendered-form
  (let [b (registered-broker)
        handler (switchboard/handler b config)
        csrf (auth/csrf-token config access-identity)
        refreshed-identity (assoc access-identity
                                  :issued-at 1100000
                                  :expires-at 2100000)]
    (with-redefs [auth/authenticate! (fn [& _] refreshed-identity)]
      (is (= 303
             (:status
               (handler
                 (form-request
                   "/messages"
                   (str "csrf=" csrf "&target=" target
                        "&message=Synthetic+request")))))))
    (is (= "Synthetic request"
           (-> (broker/poll! b {:runner-id "runner-a"
                                :token "synthetic-runner-token"})
               :messages first :body)))))

(deftest same-origin-fetch-metadata-survives-a-missing-origin-header
  (authenticated
    (fn []
      (let [b (registered-broker)
            handler (switchboard/handler b config)
            csrf (auth/csrf-token config access-identity)
            request (-> (form-request
                          "/messages"
                          (str "csrf=" csrf "&target=" target
                               "&message=Synthetic+request"))
                        (update :headers dissoc "origin")
                        (assoc-in [:headers "host"] "control.invalid")
                        (assoc-in [:headers "sec-fetch-site"] "same-origin"))]
        (is (= 303 (:status (handler request))))
        (is (= "Synthetic request"
               (-> (broker/poll! b {:runner-id "runner-a"
                                    :token "synthetic-runner-token"})
                   :messages first :body)))))))

(deftest routing-posts-require-origin-and-access-bound-csrf
  (authenticated
    (fn []
      (let [b (registered-broker)
            handler (switchboard/handler b config)
            csrf (auth/csrf-token config access-identity)
            base (form-request "/messages"
                               (str "csrf=" csrf "&target=" target
                                    "&message=Synthetic+request"))
            diagnostic-output (StringWriter.)
            _ (binding [*err* diagnostic-output]
                (doseq [request
                        [(assoc-in base [:headers "origin"]
                                   "https://other.invalid")
                         (-> base
                             (update :headers dissoc "origin")
                             (assoc-in [:headers "host"] "other.invalid")
                             (assoc-in [:headers "sec-fetch-site"]
                                       "same-origin"))
                         (-> base
                             (update :headers dissoc "origin")
                             (assoc-in [:headers "host"] "control.invalid")
                             (assoc-in [:headers "sec-fetch-site"]
                                       "cross-site"))
                         (update base :headers dissoc "origin")
                         (assoc base :body
                                (body
                                  (str "csrf=wrong&target=" target
                                       "&message=Synthetic+request")))]]
                  (is (= 403 (:status (handler request))))))
            diagnostics (str diagnostic-output)]
        (is (str/includes? diagnostics "rejected form: origin"))
        (is (str/includes? diagnostics "rejected form: token"))
        (is (not (str/includes? diagnostics "https://other.invalid")))
        (is (not (str/includes? diagnostics csrf)))
        (is (empty? (:messages (broker/poll! b {:runner-id "runner-a"
                                                :token "synthetic-runner-token"}))))
        (is (= 422 (:status
                     (handler (assoc base :body
                                     (body (str "csrf=" csrf "&target=" target
                                                "&message=%2Fmcp+reconnect+cch")))))))
        (is (empty? (:messages (broker/poll! b {:runner-id "runner-a"
                                                :token "synthetic-runner-token"}))))))))

(deftest human-and-runner-listeners-have-disjoint-routes-and-credentials
  (authenticated
    (fn []
      (let [b (registered-broker)
            human (broker-http/web-handler b config)
            runner (broker-http/handler b)]
        (is (= 404 (:status (human (access-request :get "/v1/sessions")))))
        (is (= 404 (:status (runner (access-request :get "/")))))
        (is (= 401 (:status (runner (access-request :get "/v1/sessions")))))))))

(deftest logout-is-csrf-protected-and-delegated-to-cloudflare-access
  (authenticated
    (fn []
      (let [handler (switchboard/handler (registered-broker) config)
            csrf (auth/csrf-token config access-identity)]
        (is (= 403 (:status (handler (form-request "/logout" "csrf=wrong")))))
        (let [response (handler (form-request "/logout" (str "csrf=" csrf)))]
          (is (= 303 (:status response)))
          (is (= "https://control.invalid/cdn-cgi/access/logout"
                 (get-in response [:headers "Location"]))))))))

(deftest blocked-identities-do-not-leak-private-details
  (let [handler (switchboard/handler (registered-broker) config)]
    (with-redefs [auth/authenticate!
                  (fn [& _]
                    (throw (ex-info "private identity detail"
                                    {:type :identity-not-allowed})))]
      (let [response (handler (access-request :get "/"))]
        (is (= 403 (:status response)))
        (is (not (str/includes? (:body response) "private identity detail")))))))
