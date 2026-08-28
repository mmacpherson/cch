(ns cch.control.web-auth
  "Google OIDC and short-lived signed browser sessions for the human control
  switchboard. Runner pairing and provider credentials never pass through this
  namespace."
  (:require [cch.control.broker :as broker]
            [cheshire.core :as json]
            [clojure.string :as str])
  (:import [com.nimbusds.jose JWSAlgorithm]
           [com.nimbusds.jose.crypto RSASSAVerifier]
           [com.nimbusds.jose.jwk JWKSet RSAKey]
           [com.nimbusds.jwt SignedJWT]
           [java.net URI URLEncoder]
           [java.net.http HttpClient HttpRequest HttpRequest$BodyPublishers
            HttpResponse$BodyHandlers]
           [java.nio.charset StandardCharsets]
           [java.security MessageDigest SecureRandom]
           [java.time Duration]
           [java.util Base64 Date]
           [javax.crypto Mac]
           [javax.crypto.spec SecretKeySpec]))

(def ^:const login-ttl-ms (* 10 60 1000))
(def ^:const default-session-ttl-ms (* 8 60 60 1000))
(def ^:const max-session-ttl-ms (* 24 60 60 1000))
(def ^:const transaction-cookie-name "__Host-cch_oidc")
(def ^:const session-cookie-name "__Host-cch_session")

(def ^:private authorization-endpoint
  "https://accounts.google.com/o/oauth2/v2/auth")
(def ^:private token-endpoint "https://oauth2.googleapis.com/token")
(def ^:private jwks-endpoint "https://www.googleapis.com/oauth2/v3/certs")
(def ^:private accepted-issuers
  #{"https://accounts.google.com" "accounts.google.com"})
(def ^:private secure-random (SecureRandom.))
(def ^:private base64url-encoder (.withoutPadding (Base64/getUrlEncoder)))
(def ^:private base64url-decoder (Base64/getUrlDecoder))

(def ^:dynamic *now-ms* #(System/currentTimeMillis))

(defn- random-token [byte-count]
  (let [value (byte-array byte-count)]
    (.nextBytes secure-random value)
    (.encodeToString base64url-encoder value)))

(defn- sha256 [value]
  (.digest (MessageDigest/getInstance "SHA-256")
           (.getBytes ^String value StandardCharsets/UTF_8)))

(defn- hmac [secret value]
  (let [mac (Mac/getInstance "HmacSHA256")]
    (.init mac (SecretKeySpec.
                 (.getBytes ^String secret StandardCharsets/UTF_8)
                 "HmacSHA256"))
    (.doFinal mac (.getBytes ^String value StandardCharsets/UTF_8))))

(defn- signed-token [config claims]
  (let [payload (-> claims json/generate-string
                    (.getBytes StandardCharsets/UTF_8)
                    (->> (.encodeToString base64url-encoder)))]
    (str payload "."
         (.encodeToString base64url-encoder
                          (hmac (:session-secret config) payload)))))

(defn- decode-token [config token expected-kind]
  (try
    (let [[payload signature & extra] (str/split (or token "") #"\." -1)
          expected (.encodeToString base64url-encoder
                                    (hmac (:session-secret config) payload))]
      (when (and payload signature (empty? extra)
                 (broker/secure-equal? expected signature))
        (let [claims (-> (.decode base64url-decoder payload)
                         (String. StandardCharsets/UTF_8)
                         (json/parse-string true))]
          (when (and (= expected-kind (:kind claims))
                     (number? (:expires-at claims))
                     (< (*now-ms*) (:expires-at claims)))
            claims))))
    (catch Exception _ nil)))

(defn- normalized-emails [raw]
  (->> (str/split (or raw "") #",")
       (map #(-> % str/trim str/lower-case))
       (remove str/blank?)
       set))

(defn- valid-origin? [value]
  (try
    (let [uri (URI/create value)
          path (.getPath uri)]
      (and (= "https" (.getScheme uri))
           (not (str/blank? (.getHost uri)))
           (nil? (.getUserInfo uri))
           (nil? (.getQuery uri))
           (nil? (.getFragment uri))
           (or (str/blank? path) (= "/" path))))
    (catch Exception _ false)))

(defn config-from-env
  "Return nil when the human webapp is entirely unconfigured. Any partial or
  insecure configuration fails startup rather than weakening runner APIs."
  ([] (config-from-env (System/getenv)))
  ([env]
   (let [keys ["CCH_CONTROL_WEB_ORIGIN"
               "CCH_CONTROL_GOOGLE_CLIENT_ID"
               "CCH_CONTROL_GOOGLE_CLIENT_SECRET"
               "CCH_CONTROL_GOOGLE_ALLOWED_EMAILS"
               "CCH_CONTROL_WEB_SESSION_SECRET"]
         configured (select-keys env keys)]
     (when (seq configured)
       (doseq [key keys]
         (when (str/blank? (get env key))
           (throw (ex-info (str key " is required when the switchboard is enabled")
                           {:type :invalid-web-config :field key}))))
       (let [origin (str/replace (get env "CCH_CONTROL_WEB_ORIGIN") #"/$" "")
             emails (normalized-emails
                      (get env "CCH_CONTROL_GOOGLE_ALLOWED_EMAILS"))
             secret (get env "CCH_CONTROL_WEB_SESSION_SECRET")
             hours-raw (get env "CCH_CONTROL_WEB_SESSION_HOURS")
             hours (if (str/blank? hours-raw) 8 (parse-long hours-raw))]
         (when-not (valid-origin? origin)
           (throw (ex-info "CCH_CONTROL_WEB_ORIGIN must be an HTTPS origin"
                           {:type :invalid-web-config
                            :field "CCH_CONTROL_WEB_ORIGIN"})))
         (when-not (and (seq emails)
                        (every? #(re-matches #"[^@\s]+@[^@\s]+" %) emails))
           (throw (ex-info "Google email allowlist must contain exact email addresses"
                           {:type :invalid-web-config
                            :field "CCH_CONTROL_GOOGLE_ALLOWED_EMAILS"})))
         (when (< (count secret) 32)
           (throw (ex-info "Browser session secret must contain at least 32 characters"
                           {:type :invalid-web-config
                            :field "CCH_CONTROL_WEB_SESSION_SECRET"})))
         (when-not (and hours (<= 1 hours 24))
           (throw (ex-info "Browser session lifetime must be between 1 and 24 hours"
                           {:type :invalid-web-config
                            :field "CCH_CONTROL_WEB_SESSION_HOURS"})))
         {:origin origin
          :client-id (get env "CCH_CONTROL_GOOGLE_CLIENT_ID")
          :client-secret (get env "CCH_CONTROL_GOOGLE_CLIENT_SECRET")
          :allowed-emails emails
          :session-secret secret
          :session-ttl-ms (min max-session-ttl-ms (* hours 60 60 1000))})))))

(defn- form-encode [values]
  (->> values
       (map (fn [[key value]]
              (str (URLEncoder/encode (name key) StandardCharsets/UTF_8)
                   "="
                   (URLEncoder/encode (str value) StandardCharsets/UTF_8))))
       (str/join "&")))

(defn- http-request! [request]
  (let [client (-> (HttpClient/newBuilder)
                   (.connectTimeout (Duration/ofSeconds 5))
                   .build)
        response (.send client request (HttpResponse$BodyHandlers/ofString))]
    (when-not (= 200 (.statusCode response))
      (throw (ex-info "Google OIDC endpoint rejected the request"
                      {:type :oidc-upstream-error
                       :status (.statusCode response)})))
    (.body response)))

(defn- exchange-code [config code verifier]
  (let [body (form-encode {:code code
                           :client_id (:client-id config)
                           :client_secret (:client-secret config)
                           :redirect_uri (str (:origin config) "/auth/google/callback")
                           :grant_type "authorization_code"
                           :code_verifier verifier})]
    (-> (HttpRequest/newBuilder (URI/create token-endpoint))
        (.timeout (Duration/ofSeconds 10))
        (.header "Content-Type" "application/x-www-form-urlencoded")
        (.POST (HttpRequest$BodyPublishers/ofString body))
        .build
        http-request!
        (json/parse-string true))))

(defn- fetch-jwks []
  (-> (HttpRequest/newBuilder (URI/create jwks-endpoint))
      (.timeout (Duration/ofSeconds 10))
      .GET
      .build
      http-request!))

(def ^:dynamic *exchange-code!* exchange-code)
(def ^:dynamic *fetch-jwks!* fetch-jwks)

(defn- date-ms [^Date value]
  (when value (.getTime value)))

(defn verify-id-token
  "Verify a Google ID token locally against Google's current signing keys and
  return only the stable subject and verified email needed by the allowlist."
  [config id-token nonce jwks-json]
  (try
    (let [jwt (SignedJWT/parse id-token)
          header (.getHeader jwt)
          _ (when-not (= JWSAlgorithm/RS256 (.getAlgorithm header))
              (throw (ex-info "Google ID token uses an unexpected algorithm"
                              {:type :invalid-id-token})))
          key (.getKeyByKeyId (JWKSet/parse jwks-json) (.getKeyID header))
          _ (when-not (instance? RSAKey key)
              (throw (ex-info "Google ID token signing key is unavailable"
                              {:type :invalid-id-token})))
          _ (when-not (.verify jwt (RSASSAVerifier.
                                     (.toRSAPublicKey ^RSAKey key)))
              (throw (ex-info "Google ID token signature is invalid"
                              {:type :invalid-id-token})))
          claims (.getJWTClaimsSet jwt)
          now (*now-ms*)
          audience (set (.getAudience claims))
          authorized-party (.getStringClaim claims "azp")
          issued-at (date-ms (.getIssueTime claims))
          not-before (date-ms (.getNotBeforeTime claims))
          expires-at (date-ms (.getExpirationTime claims))
          token-nonce (.getStringClaim claims "nonce")
          subject (.getSubject claims)
          email (some-> (.getStringClaim claims "email") str/lower-case)
          verified (.getBooleanClaim claims "email_verified")]
      (when-not (contains? accepted-issuers (.getIssuer claims))
        (throw (ex-info "Google ID token issuer is invalid"
                        {:type :invalid-id-token})))
      (when-not (contains? audience (:client-id config))
        (throw (ex-info "Google ID token audience is invalid"
                        {:type :invalid-id-token})))
      (when (and (> (count audience) 1)
                 (not= (:client-id config) authorized-party))
        (throw (ex-info "Google ID token authorized party is invalid"
                        {:type :invalid-id-token})))
      (when-not (and issued-at expires-at
                     (<= issued-at (+ now 60000))
                     (or (nil? not-before) (<= not-before (+ now 60000)))
                     (< now expires-at))
        (throw (ex-info "Google ID token is expired or not yet valid"
                        {:type :invalid-id-token})))
      (when-not (broker/secure-equal? nonce token-nonce)
        (throw (ex-info "Google ID token nonce is invalid"
                        {:type :invalid-id-token})))
      (when-not (and (true? verified) (contains? (:allowed-emails config) email))
        (throw (ex-info "Google identity is not allowed"
                        {:type :identity-not-allowed})))
      (when (str/blank? subject)
        (throw (ex-info "Google ID token subject is missing"
                        {:type :invalid-id-token})))
      {:subject subject :email email})
    (catch clojure.lang.ExceptionInfo error (throw error))
    (catch Exception error
      (throw (ex-info "Google ID token is invalid"
                      {:type :invalid-id-token} error)))))

(defn begin-login [config]
  (let [now (*now-ms*)
        state (random-token 32)
        nonce (random-token 32)
        verifier (random-token 64)
        challenge (.encodeToString base64url-encoder (sha256 verifier))
        transaction (signed-token config {:kind "oidc"
                                          :state state
                                          :nonce nonce
                                          :verifier verifier
                                          :expires-at (+ now login-ttl-ms)})
        query (form-encode {:client_id (:client-id config)
                            :redirect_uri (str (:origin config)
                                               "/auth/google/callback")
                            :response_type "code"
                            :scope "openid email profile"
                            :state state
                            :nonce nonce
                            :code_challenge challenge
                            :code_challenge_method "S256"})]
    {:location (str authorization-endpoint "?" query)
     :transaction transaction}))

(defn complete-login! [config {:keys [code state transaction]}]
  (let [claims (decode-token config transaction "oidc")]
    (when-not (and claims (broker/secure-equal? (:state claims) state))
      (throw (ex-info "OIDC login state is invalid or expired"
                      {:type :invalid-login-state})))
    (when (str/blank? code)
      (throw (ex-info "OIDC authorization code is missing"
                      {:type :invalid-login-state})))
    (let [tokens (*exchange-code!* config code (:verifier claims))
          id-token (:id_token tokens)]
      (when (str/blank? id-token)
        (throw (ex-info "Google token response omitted the ID token"
                        {:type :invalid-id-token})))
      (verify-id-token config id-token (:nonce claims) (*fetch-jwks!*)))))

(defn new-session [config identity]
  (let [now (*now-ms*)]
    (signed-token config {:kind "session"
                          :subject (:subject identity)
                          :email (:email identity)
                          :csrf (random-token 32)
                          :issued-at now
                          :expires-at (+ now (:session-ttl-ms config))})))

(defn session [config token]
  (decode-token config token "session"))

(defn csrf-valid? [session value]
  (and (string? value)
       (broker/secure-equal? (:csrf session) value)))

(defn cookie
  ([name value max-age]
   (str name "=" value "; Path=/; Max-Age=" max-age
        "; Secure; HttpOnly; SameSite=Lax"))
  ([name]
   (str name "=; Path=/; Max-Age=0; Secure; HttpOnly; SameSite=Lax")))
