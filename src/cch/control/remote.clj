(ns cch.control.remote
  "Authenticated HTTP client used by a paired runner and its local MCP calls."
  (:require [cheshire.core :as json]
            [clojure.string :as str]
            [org.httpkit.client :as http])
  (:import [java.net URI]))

(def ^:dynamic *request!* http/request)

(defn config-from-env
  "Return nil when remote routing is not configured. Partial or unsafe config
  is rejected so a pairing token is never sent over public cleartext HTTP."
  ([] (config-from-env (System/getenv)))
  ([env]
   (let [url (get env "CCH_CONTROL_BROKER_URL")
         runner-id (get env "CCH_CONTROL_RUNNER_ID")
         token (get env "CCH_CONTROL_RUNNER_TOKEN")
         present (remove str/blank? [url runner-id token])]
     (cond
       (empty? present) nil
       (not= 3 (count present))
       (throw (ex-info "Broker URL, runner id, and runner token must be configured together"
                       {:type :invalid-runner-config}))
       :else
       (let [uri (URI. url)
             loopback? (contains? #{"localhost" "127.0.0.1" "::1"} (.getHost uri))]
         (when-not (or (= "https" (.getScheme uri))
                       (and (= "http" (.getScheme uri)) loopback?))
           (throw (ex-info "Broker URL must use HTTPS (HTTP is allowed only on loopback)"
                           {:type :insecure-broker-url})))
         {:url (str/replace url #"/+$" "")
          :runner-id runner-id
          :token token})))))

(defn configured? []
  (boolean (config-from-env)))

(defn- request-json!
  [config method path payload]
  (let [request (cond-> {:url (str (:url config) path)
                         :method method
                         :headers {"Accept" "application/json"
                                   "Authorization" (str "Bearer " (:token config))
                                   "X-CCH-Runner-ID" (:runner-id config)}
                         :timeout 5000}
                  payload (assoc :body (json/generate-string payload)
                                 :headers {"Accept" "application/json"
                                           "Content-Type" "application/json"
                                           "Authorization" (str "Bearer " (:token config))
                                           "X-CCH-Runner-ID" (:runner-id config)}))]
    (try
      (let [{:keys [status body error]} @(*request!* request)
            parsed (try
                     (when-not (str/blank? body)
                       (json/parse-string body true))
                     (catch Exception _ nil))]
        (cond
          error
          (throw (ex-info "Broker request failed"
                          {:type :broker-unavailable} error))

          (<= 200 status 299)
          parsed

          :else
          (throw (ex-info (or (:message parsed) "Broker rejected request")
                          {:type (some-> (:type parsed) keyword)
                           :status status}))))
      (catch clojure.lang.ExceptionInfo error
        (throw error))
      (catch Exception error
        (throw (ex-info "Broker is unavailable"
                        {:type :broker-unavailable} error))))))

(defn register! [config sessions]
  (request-json! config :post "/v1/runners/register"
                 {:runner-id (:runner-id config) :sessions sessions}))

(defn sessions [config]
  (:sessions (request-json! config :get "/v1/sessions" nil)))

(defn enqueue! [config envelope]
  (request-json! config :post "/v1/messages"
                 (assoc envelope :runner-id (:runner-id config))))

(defn poll! [config]
  (:messages (request-json! config :post "/v1/runners/poll"
                            {:runner-id (:runner-id config)})))

(defn ack! [config message-id status & [failure]]
  (request-json! config :post "/v1/messages/ack"
                 (cond-> {:runner-id (:runner-id config)
                          :message-id message-id
                          :status status}
                   failure (assoc :failure failure))))

(defn message-status [config message-id]
  (:message (request-json! config :post "/v1/messages/status"
                           {:runner-id (:runner-id config)
                            :message-id message-id})))
