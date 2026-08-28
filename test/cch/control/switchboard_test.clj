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
  (let [b (broker/new-broker {"runner-a" "synthetic-runner-token"})]
    (broker/register!
      b {:runner-id "runner-a"
         :token "synthetic-runner-token"
         :sessions [{:id target :agent "claude" :status "waiting"
                     :native-url "https://claude.ai/code/session_synthetic123"
                     :available true :cwd "/private/not-federated"
                     :name "Private session label"}]})
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
    (authenticated
      #(is (= 200 (:status (handler (access-request :get "/"))))))))

(deftest switchboard-renders-only-sanitized-presence-and-native-authority
  (authenticated
    (fn []
      (let [response ((switchboard/handler (registered-broker) config)
                      (access-request :get "/"))
            page (:body response)]
        (is (= 200 (:status response)))
        (is (str/includes? page target))
        (is (str/includes? page "Needs you"))
        (is (str/includes? page "Open this Claude session"))
        (is (str/includes? page
                           "https://claude.ai/code/session_synthetic123"))
        (is (not (str/includes? page "href=\"https://claude.ai/code\"")))
        (is (not (str/includes? page "Native authority")))
        (is (not (str/includes? page "/private/not-federated")))
        (is (not (str/includes? page "Private session label")))
        (is (not (str/includes? page assertion)))
        (is (= "no-store" (get-in response [:headers "Cache-Control"])))
        (is (str/includes? (get-in response [:headers "Content-Security-Policy"])
                           "frame-ancestors 'none'"))))))

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
                              :get "/"
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

(deftest routing-posts-require-origin-and-access-bound-csrf
  (authenticated
    (fn []
      (let [b (registered-broker)
            handler (switchboard/handler b config)
            csrf (auth/csrf-token config access-identity)
            base (form-request "/messages"
                               (str "csrf=" csrf "&target=" target
                                    "&message=Synthetic+request"))]
        (doseq [request [(assoc-in base [:headers "origin"]
                                   "https://other.invalid")
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
