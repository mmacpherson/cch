(ns cch.control.switchboard-test
  (:require [cch.control.broker :as broker]
            [cch.control.broker-http :as broker-http]
            [cch.control.switchboard :as switchboard]
            [cch.control.web-auth :as auth]
            [clojure.string :as str]
            [clojure.test :refer [deftest is]])
  (:import [java.io ByteArrayInputStream]
           [java.nio.charset StandardCharsets]))

(def config
  {:origin "https://control.invalid"
   :client-id "synthetic-client-id"
   :client-secret "synthetic-client-secret"
   :allowed-emails #{"operator@example.invalid"}
   :session-secret "synthetic-session-secret-with-more-than-32-characters"
   :session-ttl-ms 3600000})

(def target "claude:00000000-0000-0000-0000-00000000000a")

(defn- registered-broker []
  (let [b (broker/new-broker {"runner-a" "synthetic-runner-token"})]
    (broker/register!
      b {:runner-id "runner-a"
         :token "synthetic-runner-token"
         :sessions [{:id target :agent "claude" :status "waiting"
                     :available true :cwd "/private/not-federated"
                     :name "Private session label"}]})
    b))

(defn- body [value]
  (ByteArrayInputStream.
    (.getBytes ^String value StandardCharsets/UTF_8)))

(defn- session-cookie [now]
  (binding [auth/*now-ms* (constantly now)]
    (str auth/session-cookie-name "="
         (auth/new-session
           config {:subject "synthetic-google-subject"
                   :email "operator@example.invalid"}))))

(defn- form-request [uri cookie value]
  {:request-method :post
   :uri uri
   :headers {"cookie" cookie
             "origin" (:origin config)
             "content-type" "application/x-www-form-urlencoded"}
   :body (body value)})

(defn- csrf-from-cookie [cookie now]
  (binding [auth/*now-ms* (constantly now)]
    (:csrf (auth/session config (second (str/split cookie #"=" 2))))))

(deftest human-pages-require-an-unexpired-session
  (let [b (registered-broker)
        handler (switchboard/handler b config)
        clock (atom 1000000)
        cookie (session-cookie @clock)]
    (is (= "/login" (get-in (handler {:request-method :get :uri "/"})
                              [:headers "Location"])))
    (binding [auth/*now-ms* #(deref clock)]
      (is (= 200 (:status (handler {:request-method :get :uri "/"
                                    :headers {"cookie" cookie}}))))
      (swap! clock + (:session-ttl-ms config))
      (is (= "/login" (get-in (handler {:request-method :get :uri "/"
                                         :headers {"cookie" cookie}})
                               [:headers "Location"]))))))

(deftest switchboard-renders-only-sanitized-presence-and-native-authority
  (let [now 2000000]
    (binding [auth/*now-ms* (constantly now)]
      (let [response ((switchboard/handler (registered-broker) config)
                      {:request-method :get :uri "/"
                       :headers {"cookie" (session-cookie now)}})
            page (:body response)]
        (is (= 200 (:status response)))
        (is (str/includes? page target))
        (is (str/includes? page "Needs you"))
        (is (str/includes? page "Open Claude Code"))
        (is (str/includes? page "provider-native interface"))
        (is (not (str/includes? page "/private/not-federated")))
        (is (not (str/includes? page "Private session label")))
        (is (= "no-store" (get-in response [:headers "Cache-Control"])))
        (is (str/includes? (get-in response [:headers "Content-Security-Policy"])
                           "frame-ancestors 'none'"))))))

(deftest valid-csrf-routes-transient-operator-text-and-reports-delivery
  (let [now 3000000
        b (registered-broker)
        handler (switchboard/handler b config)
        cookie (session-cookie now)
        csrf (csrf-from-cookie cookie now)]
    (binding [auth/*now-ms* (constantly now)]
      (let [response (handler
                       (form-request
                         "/messages" cookie
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
        (let [page (:body (handler {:request-method :get :uri "/"
                                    :query-string (str "message-id=" message-id)
                                    :headers {"cookie" cookie}}))]
          (is (str/includes? page "Message delivered"))
          (is (not (str/includes? page "Synthetic operator request"))))))))

(deftest routing-posts-require-origin-and-session-bound-csrf
  (let [now 4000000
        b (registered-broker)
        handler (switchboard/handler b config)
        cookie (session-cookie now)
        csrf (csrf-from-cookie cookie now)
        base (form-request "/messages" cookie
                           (str "csrf=" csrf "&target=" target
                                "&message=Synthetic+request"))]
    (binding [auth/*now-ms* (constantly now)]
      (doseq [request [(assoc-in base [:headers "origin"] "https://other.invalid")
                       (assoc base :body
                              (body (str "csrf=wrong&target=" target
                                         "&message=Synthetic+request")))]]
        (is (= 403 (:status (handler request)))))
      (is (empty? (:messages (broker/poll! b {:runner-id "runner-a"
                                              :token "synthetic-runner-token"}))))
      (is (= 422 (:status
                   (handler (assoc base :body
                                   (body (str "csrf=" csrf "&target=" target
                                              "&message=%2Fmcp+reconnect+cch")))))))
      (is (empty? (:messages (broker/poll! b {:runner-id "runner-a"
                                              :token "synthetic-runner-token"})))))))

(deftest browser-cookie-never-authorizes-runner-api
  (let [now 5000000
        b (registered-broker)
        handler (broker-http/handler b config)
        cookie (session-cookie now)]
    (binding [auth/*now-ms* (constantly now)]
      (is (= 200 (:status (handler {:request-method :get :uri "/"
                                    :headers {"cookie" cookie}}))))
      (is (= 401 (:status (handler {:request-method :get
                                    :uri "/v1/sessions"
                                    :headers {"cookie" cookie}})))))))

(deftest login-and-callback-never-expose-secrets-or-blocked-identities
  (let [b (registered-broker)
        handler (switchboard/handler b config)
        login (handler {:request-method :get :uri "/login"})]
    (is (= 303 (:status login)))
    (is (str/starts-with? (get-in login [:headers "Location"])
                          "https://accounts.google.com/"))
    (is (not (str/includes? (str login) (:client-secret config))))
    (is (not (str/includes? (str login) (:session-secret config))))
    (with-redefs [auth/complete-login!
                  (fn [& _] {:subject "synthetic-google-subject"
                             :email "operator@example.invalid"})]
      (let [response (handler {:request-method :get
                               :uri "/auth/google/callback"
                               :query-string "code=synthetic&state=synthetic"
                               :headers {"cookie" "__Host-cch_oidc=synthetic"}})
            cookies (get-in response [:headers "Set-Cookie"])]
        (is (= 303 (:status response)))
        (is (= 2 (count cookies)))
        (is (some #(str/starts-with? % "__Host-cch_session=") cookies))
        (is (every? #(str/includes? % "Secure; HttpOnly; SameSite=Lax") cookies))))
    (with-redefs [auth/complete-login!
                  (fn [& _]
                    (throw (ex-info "private identity detail"
                                    {:type :identity-not-allowed})))]
      (let [response (handler {:request-method :get
                               :uri "/auth/google/callback"
                               :query-string "code=synthetic&state=synthetic"
                               :headers {"cookie" "__Host-cch_oidc=synthetic"}})]
        (is (= 403 (:status response)))
        (is (not (str/includes? (:body response) "private identity detail")))))))
