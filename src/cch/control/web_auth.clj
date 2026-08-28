(ns cch.control.web-auth
  "Cloudflare Access authentication and stateless CSRF protection for the
  human switchboard. Runner pairing and provider credentials never pass
  through this namespace."
  (:require [cch.control.broker :as broker]
            [clojure.string :as str])
  (:import [com.nimbusds.jose JWSAlgorithm]
           [com.nimbusds.jose.crypto RSASSAVerifier]
           [com.nimbusds.jose.jwk JWKSet RSAKey]
           [com.nimbusds.jwt SignedJWT]
           [java.net URI]
           [java.net.http HttpClient HttpRequest HttpResponse$BodyHandlers]
           [java.nio.charset StandardCharsets]
           [java.time Duration]
           [java.util Base64 Date]
           [javax.crypto Mac]
           [javax.crypto.spec SecretKeySpec]))

(def ^:const jwks-cache-ttl-ms (* 5 60 1000))
(def ^:private clock-skew-ms 60000)
(def ^:private base64url-encoder (.withoutPadding (Base64/getUrlEncoder)))
(defonce ^:private jwks-cache (atom {}))
(def ^:dynamic *now-ms* #(System/currentTimeMillis))

(defn- normalized-emails [raw]
  (->> (str/split (or raw "") #",")
       (map #(-> % str/trim str/lower-case))
       (remove str/blank?)
       set))

(defn- https-origin? [value required-host-suffix]
  (try
    (let [uri (URI/create value)
          host (some-> (.getHost uri) str/lower-case)
          path (.getPath uri)]
      (and (= "https" (.getScheme uri))
           (not (str/blank? host))
           (or (nil? required-host-suffix)
               (str/ends-with? host required-host-suffix))
           (nil? (.getUserInfo uri))
           (nil? (.getQuery uri))
           (nil? (.getFragment uri))
           (or (str/blank? path) (= "/" path))))
    (catch Exception _ false)))

(defn config-from-env
  "Return nil when the human switchboard is entirely unconfigured. Any
  partial or insecure configuration fails startup rather than weakening the
  runner API."
  ([] (config-from-env (System/getenv)))
  ([env]
   (let [required ["CCH_CONTROL_WEB_ORIGIN"
                   "CCH_CONTROL_CLOUDFLARE_ISSUER"
                   "CCH_CONTROL_CLOUDFLARE_AUDIENCE"
                   "CCH_CONTROL_WEB_ALLOWED_EMAILS"
                   "CCH_CONTROL_WEB_SESSION_SECRET"]
         configured (select-keys env required)]
     (when (seq configured)
       (doseq [key required]
         (when (str/blank? (get env key))
           (throw (ex-info (str key " is required when the switchboard is enabled")
                           {:type :invalid-web-config :field key}))))
       (let [origin (str/replace (get env "CCH_CONTROL_WEB_ORIGIN") #"/$" "")
             issuer (str/replace (get env "CCH_CONTROL_CLOUDFLARE_ISSUER") #"/$" "")
             audience (get env "CCH_CONTROL_CLOUDFLARE_AUDIENCE")
             emails (normalized-emails (get env "CCH_CONTROL_WEB_ALLOWED_EMAILS"))
             secret (get env "CCH_CONTROL_WEB_SESSION_SECRET")
             listen-host (or (not-empty (get env "CCH_CONTROL_WEB_HOST"))
                             "127.0.0.1")
             port-raw (get env "CCH_CONTROL_WEB_PORT")
             listen-port (if (str/blank? port-raw) 8788 (parse-long port-raw))]
         (when-not (https-origin? origin nil)
           (throw (ex-info "CCH_CONTROL_WEB_ORIGIN must be an HTTPS origin"
                           {:type :invalid-web-config
                            :field "CCH_CONTROL_WEB_ORIGIN"})))
         (when-not (https-origin? issuer ".cloudflareaccess.com")
           (throw (ex-info
                    "CCH_CONTROL_CLOUDFLARE_ISSUER must be an HTTPS Cloudflare Access origin"
                    {:type :invalid-web-config
                     :field "CCH_CONTROL_CLOUDFLARE_ISSUER"})))
         (when-not (re-matches #"[A-Za-z0-9_-]{16,256}" audience)
           (throw (ex-info "Cloudflare Access audience is invalid"
                           {:type :invalid-web-config
                            :field "CCH_CONTROL_CLOUDFLARE_AUDIENCE"})))
         (when-not (and (seq emails)
                        (every? #(re-matches #"[^@\s]+@[^@\s]+" %) emails))
           (throw (ex-info "Web allowlist must contain exact email addresses"
                           {:type :invalid-web-config
                            :field "CCH_CONTROL_WEB_ALLOWED_EMAILS"})))
         (when (< (count secret) 32)
           (throw (ex-info "Web session secret must contain at least 32 characters"
                           {:type :invalid-web-config
                            :field "CCH_CONTROL_WEB_SESSION_SECRET"})))
         (when (str/blank? listen-host)
           (throw (ex-info "Web listener host must not be blank"
                           {:type :invalid-web-config
                            :field "CCH_CONTROL_WEB_HOST"})))
         (when-not (and listen-port (<= 1 listen-port 65535))
           (throw (ex-info "Web listener port must be between 1 and 65535"
                           {:type :invalid-web-config
                            :field "CCH_CONTROL_WEB_PORT"})))
         {:origin origin
          :issuer issuer
          :audience audience
          :allowed-emails emails
          :session-secret secret
          :listen-host listen-host
          :listen-port listen-port
          :jwks-url (str issuer "/cdn-cgi/access/certs")})))))

(defn- http-get! [url]
  (let [client (-> (HttpClient/newBuilder)
                   (.connectTimeout (Duration/ofSeconds 5))
                   .build)
        request (-> (HttpRequest/newBuilder (URI/create url))
                    (.timeout (Duration/ofSeconds 10))
                    .GET
                    .build)
        response (.send client request (HttpResponse$BodyHandlers/ofString))]
    (when-not (= 200 (.statusCode response))
      (throw (ex-info "Cloudflare Access key endpoint rejected the request"
                      {:type :access-upstream-error
                       :status (.statusCode response)})))
    (.body response)))

(def ^:dynamic *fetch-jwks!* (fn [config] (http-get! (:jwks-url config))))

(defn- cached-jwks! [config force-refresh?]
  (let [key (:jwks-url config)
        now (*now-ms*)
        cached (get @jwks-cache key)]
    (if (and (not force-refresh?) cached (< now (:refresh-after cached)))
      (:value cached)
      (let [value (*fetch-jwks!* config)]
        (swap! jwks-cache assoc key {:value value
                                     :refresh-after (+ now jwks-cache-ttl-ms)})
        value))))

(defn- date-ms [^Date value]
  (when value (.getTime value)))

(defn verify-access-token
  "Verify one Cloudflare Access application assertion and return only the
  identity fields required by the switchboard."
  [config token jwks-json]
  (try
    (let [jwt (SignedJWT/parse token)
          header (.getHeader jwt)
          _ (when-not (= JWSAlgorithm/RS256 (.getAlgorithm header))
              (throw (ex-info "Access token uses an unexpected algorithm"
                              {:type :invalid-access-token})))
          key (.getKeyByKeyId (JWKSet/parse jwks-json) (.getKeyID header))
          _ (when-not (instance? RSAKey key)
              (throw (ex-info "Access token signing key is unavailable"
                              {:type :access-key-unavailable})))
          _ (when-not (.verify jwt (RSASSAVerifier. (.toRSAPublicKey ^RSAKey key)))
              (throw (ex-info "Access token signature is invalid"
                              {:type :invalid-access-token})))
          claims (.getJWTClaimsSet jwt)
          now (*now-ms*)
          audience (set (.getAudience claims))
          issued-at (date-ms (.getIssueTime claims))
          not-before (date-ms (.getNotBeforeTime claims))
          expires-at (date-ms (.getExpirationTime claims))
          subject (.getSubject claims)
          email (some-> (.getStringClaim claims "email") str/lower-case)]
      (when-not (= (:issuer config) (.getIssuer claims))
        (throw (ex-info "Access token issuer is invalid"
                        {:type :invalid-access-token})))
      (when-not (contains? audience (:audience config))
        (throw (ex-info "Access token audience is invalid"
                        {:type :invalid-access-token})))
      (when-not (and issued-at expires-at
                     (<= issued-at (+ now clock-skew-ms))
                     (or (nil? not-before)
                         (<= not-before (+ now clock-skew-ms)))
                     (< now expires-at))
        (throw (ex-info "Access token is expired or not yet valid"
                        {:type :invalid-access-token})))
      (when-not (contains? (:allowed-emails config) email)
        (throw (ex-info "Access identity is not allowed"
                        {:type :identity-not-allowed})))
      (when (str/blank? subject)
        (throw (ex-info "Access token subject is missing"
                        {:type :invalid-access-token})))
      {:subject subject
       :email email
       :issued-at issued-at
       :expires-at expires-at})
    (catch clojure.lang.ExceptionInfo error (throw error))
    (catch Exception error
      (throw (ex-info "Cloudflare Access token is invalid"
                      {:type :invalid-access-token} error)))))

(defn authenticate!
  "Authenticate a Ring request from the assertion injected by Cloudflare
  Access. A missing assertion is never replaced by a browser cookie or runner
  credential."
  [config request]
  (let [token (get-in request [:headers "cf-access-jwt-assertion"])]
    (when (str/blank? token)
      (throw (ex-info "Cloudflare Access assertion is required"
                      {:type :access-required})))
    (try
      (verify-access-token config token (cached-jwks! config false))
      (catch clojure.lang.ExceptionInfo error
        (if (= :access-key-unavailable (:type (ex-data error)))
          (verify-access-token config token (cached-jwks! config true))
          (throw error))))))

(defn- hmac [secret value]
  (let [mac (Mac/getInstance "HmacSHA256")]
    (.init mac (SecretKeySpec.
                 (.getBytes ^String secret StandardCharsets/UTF_8)
                 "HmacSHA256"))
    (.doFinal mac (.getBytes ^String value StandardCharsets/UTF_8))))

(defn- csrf-material [identity]
  (str (:subject identity) "\u0000" (:email identity) "\u0000"
       (:issued-at identity) "\u0000" (:expires-at identity)))

(defn csrf-token [config identity]
  (.encodeToString base64url-encoder
                   (hmac (:session-secret config) (csrf-material identity))))

(defn csrf-valid? [config identity value]
  (and (string? value)
       (broker/secure-equal? (csrf-token config identity) value)))

(defn logout-location [config]
  (str (:origin config) "/cdn-cgi/access/logout"))
