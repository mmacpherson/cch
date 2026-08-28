(ns cch.control.broker-http
  "Small JSON/HTTP boundary around the disposable in-memory broker."
  (:require [cch.control.broker-api :as broker]
            [cch.control.switchboard :as switchboard]
            [cheshire.core :as json]
            [clojure.string :as str]
            [org.httpkit.server :as httpkit]))

(defn- json-response
  ([status value]
   {:status status
    :headers {"Content-Type" "application/json; charset=utf-8"
              "Cache-Control" "no-store"}
    :body (json/generate-string value)}))

(defn- read-json [request]
  (if-let [body (:body request)]
    (json/parse-string (slurp body) true)
    {}))

(defn- bearer-token [request]
  (some-> (get-in request [:headers "authorization"])
          (str/replace-first #"^Bearer " "")
          not-empty))

(defn- credentials [request payload]
  {:runner-id (or (:runner-id payload)
                  (get-in request [:headers "x-cch-runner-id"]))
   :token (bearer-token request)})

(defn- error-status [type]
  (case type
    :unauthorized 401
    :forbidden 403
    :unknown-session 404
    :unknown-message 404
    :invalid-alias 422
    :route-conflict 409
    :message-id-conflict 409
    :runner-not-registered 409
    :message-too-large 413
    400))

(defn handler
  "Build the Ring handler for one Broker. Error responses expose stable types,
  never credentials, bodies, paths, or exception data."
  [b]
  (fn [{:keys [request-method uri] :as request}]
    (try
      (cond
        (and (= :get request-method) (= "/health" uri))
        (json-response 200 (broker/broker-summary b))

        (and (= :post request-method) (= "/v1/runners/register" uri))
        (let [payload (read-json request)]
          (json-response 200
                         (broker/register-runner!
                           b (merge payload (credentials request payload)))))

        (and (= :get request-method) (= "/v1/sessions" uri))
        (let [{:keys [runner-id token]} (credentials request {})]
          (broker/authorize-runner! b runner-id token)
          (json-response 200 {:sessions (broker/active-sessions b)}))

        (and (= :post request-method) (= "/v1/sessions/alias" uri))
        (let [payload (read-json request)]
          (json-response 200
                         {:session
                          (broker/set-session-alias!
                            b (merge payload (credentials request payload)))}))

        (and (= :post request-method) (= "/v1/messages" uri))
        (let [payload (read-json request)]
          (json-response 202
                         (broker/enqueue-message!
                           b (merge payload (credentials request payload)))))

        (and (= :post request-method) (= "/v1/runners/poll" uri))
        (let [payload (read-json request)]
          (json-response 200
                         (broker/poll-messages!
                           b (merge payload (credentials request payload)))))

        (and (= :post request-method) (= "/v1/messages/ack" uri))
        (let [payload (read-json request)]
          (json-response 200
                         (broker/ack-message!
                           b (merge payload (credentials request payload)))))

        (and (= :post request-method) (= "/v1/messages/status" uri))
        (let [payload (read-json request)
              {:keys [runner-id token]} (credentials request payload)]
          (if-let [message (broker/message-metadata
                             b runner-id token (:message-id payload))]
            (json-response 200 {:message message})
            (json-response 404 {:type "unknown-message"
                                :message "Unknown message"})))

        :else
        (json-response 404 {:type "not-found" :message "Not found"}))
      (catch clojure.lang.ExceptionInfo error
        (let [type (or (:type (ex-data error)) :invalid-request)]
          (json-response (error-status type)
                         {:type (name type) :message (.getMessage error)})))
      (catch Exception _
        (json-response 400 {:type "invalid-request"
                            :message "Invalid request"})))))

(defn web-handler
  "Build the physically separate human listener. Runner and health endpoints
  are absent even when a request carries a valid Access assertion."
  [b config]
  (let [human (switchboard/handler b config)]
    (fn [request]
      (or (human request)
          {:status 404
           :headers {"Content-Type" "text/plain; charset=utf-8"
                     "Cache-Control" "no-store"}
           :body "Not found"}))))

(defn start!
  "Start a broker listener. TLS is intentionally terminated by the private
  overlay/reverse proxy; the default listener is loopback-only."
  [b {:keys [host port] :or {host "127.0.0.1" port 8787}}]
  (let [stop (httpkit/run-server (handler b)
                                 {:ip host :port port
                                  :max-body (* 64 1024)})]
    {:stop stop :host host :port port}))

(defn start-web!
  "Start the Cloudflare Tunnel-only human listener separately from the runner
  API listener."
  [b {:keys [listen-host listen-port] :as config}]
  (let [stop (httpkit/run-server (web-handler b config)
                                 {:ip listen-host :port listen-port
                                  :max-body (* 16 1024)})]
    {:stop stop :host listen-host :port listen-port}))

(defn runner-tokens-from-env
  "Read the broker's pairing map from one environment value. The value is
  never printed or written by cch."
  ([] (runner-tokens-from-env (System/getenv)))
  ([env]
   (let [raw (get env "CCH_CONTROL_RUNNER_TOKENS")]
     (when (str/blank? raw)
       (throw (ex-info "CCH_CONTROL_RUNNER_TOKENS must be a JSON object"
                       {:type :missing-broker-config})))
     (let [tokens (json/parse-string raw)]
       (when-not (and (map? tokens)
                      (seq tokens)
                      (every? (fn [[runner-id token]]
                                (and (not (str/blank? runner-id))
                                     (not (str/blank? token))))
                              tokens))
         (throw (ex-info "CCH_CONTROL_RUNNER_TOKENS must map runner ids to tokens"
                         {:type :invalid-broker-config})))
       tokens))))
