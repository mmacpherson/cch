(ns cch.control.web-auth-test
  (:require [cch.control.web-auth :as auth]
            [clojure.string :as str]
            [clojure.test :refer [deftest is]])
  (:import [com.nimbusds.jose JWSAlgorithm JWSHeader$Builder]
           [com.nimbusds.jose.crypto RSASSASigner]
           [com.nimbusds.jose.jwk JWKSet]
           [com.nimbusds.jose.jwk.gen RSAKeyGenerator]
           [com.nimbusds.jwt JWTClaimsSet$Builder SignedJWT]
           [java.util Date]))

(def config
  {:origin "https://control.invalid"
   :client-id "synthetic-client-id"
   :client-secret "synthetic-client-secret"
   :allowed-emails #{"operator@example.invalid"}
   :session-secret "synthetic-session-secret-with-more-than-32-characters"
   :session-ttl-ms 3600000})

(defn- signing-key []
  (-> (RSAKeyGenerator. 2048)
      (.keyID "synthetic-key")
      .generate))

(defn- id-token
  [key {:keys [now nonce audience email verified issuer subject]
        :or {audience "synthetic-client-id"
             email "operator@example.invalid"
             verified true
             issuer "https://accounts.google.com"
             subject "synthetic-google-subject"}}]
  (let [claims (-> (JWTClaimsSet$Builder.)
                   (.issuer issuer)
                   (.subject subject)
                   (.audience audience)
                   (.issueTime (Date. now))
                   (.expirationTime (Date. (+ now 300000)))
                   (.claim "nonce" nonce)
                   (.claim "email" email)
                   (.claim "email_verified" verified)
                   .build)
        jwt (SignedJWT. (-> (JWSHeader$Builder. JWSAlgorithm/RS256)
                            (.keyID (.getKeyID key))
                            .build)
                        claims)]
    (.sign jwt (RSASSASigner. key))
    (.serialize jwt)))

(defn- public-jwks [key]
  (.toString (JWKSet. (.toPublicJWK key))))

(deftest web-config-is-all-or-nothing-and-exact-email-only
  (is (nil? (auth/config-from-env {})))
  (let [env {"CCH_CONTROL_WEB_ORIGIN" "https://control.invalid/"
             "CCH_CONTROL_GOOGLE_CLIENT_ID" "synthetic-client-id"
             "CCH_CONTROL_GOOGLE_CLIENT_SECRET" "synthetic-client-secret"
             "CCH_CONTROL_GOOGLE_ALLOWED_EMAILS"
             " Operator@Example.invalid, second@example.invalid "
             "CCH_CONTROL_WEB_SESSION_SECRET"
             "synthetic-session-secret-with-more-than-32-characters"}]
    (is (= #{"operator@example.invalid" "second@example.invalid"}
           (:allowed-emails (auth/config-from-env env))))
    (is (= "https://control.invalid" (:origin (auth/config-from-env env)))))
  (doseq [env [{"CCH_CONTROL_WEB_ORIGIN" "https://control.invalid"}
               {"CCH_CONTROL_WEB_ORIGIN" "http://control.invalid"
                "CCH_CONTROL_GOOGLE_CLIENT_ID" "id"
                "CCH_CONTROL_GOOGLE_CLIENT_SECRET" "secret"
                "CCH_CONTROL_GOOGLE_ALLOWED_EMAILS" "operator@example.invalid"
                "CCH_CONTROL_WEB_SESSION_SECRET" (apply str (repeat 40 "x"))}
               {"CCH_CONTROL_WEB_ORIGIN" "https://control.invalid"
                "CCH_CONTROL_GOOGLE_CLIENT_ID" "id"
                "CCH_CONTROL_GOOGLE_CLIENT_SECRET" "secret"
                "CCH_CONTROL_GOOGLE_ALLOWED_EMAILS" "*.example.invalid"
                "CCH_CONTROL_WEB_SESSION_SECRET" (apply str (repeat 40 "x"))}]]
    (is (= :invalid-web-config
           (try (auth/config-from-env env) nil
                (catch clojure.lang.ExceptionInfo error
                  (:type (ex-data error))))))))

(deftest google-id-token-verification-checks-signature-claims-and-allowlist
  (let [now 1000000
        key (signing-key)
        jwks (public-jwks key)]
    (binding [auth/*now-ms* (constantly now)]
      (is (= {:subject "synthetic-google-subject"
              :email "operator@example.invalid"}
             (auth/verify-id-token
               config (id-token key {:now now :nonce "nonce-1"})
               "nonce-1" jwks)))
      (doseq [[changes nonce expected]
              [[{:audience "different-client"} "nonce-1" :invalid-id-token]
               [{:email "blocked@example.invalid"} "nonce-1"
                :identity-not-allowed]
               [{:verified false} "nonce-1" :identity-not-allowed]
               [{} "different-nonce" :invalid-id-token]]]
        (is (= expected
               (try
                 (auth/verify-id-token
                   config (id-token key (merge {:now now :nonce "nonce-1"}
                                               changes))
                   nonce jwks)
                 nil
                 (catch clojure.lang.ExceptionInfo error
                   (:type (ex-data error))))))))))

(deftest login-flow-binds-state-pkce-and-nonce
  (let [now 2000000
        key (signing-key)]
    (binding [auth/*now-ms* (constantly now)]
      (let [{:keys [location transaction]} (auth/begin-login config)
            claims (#'auth/decode-token config transaction "oidc")
            exchanged (atom nil)]
        (is (str/starts-with? location "https://accounts.google.com/"))
        (is (str/includes? location "code_challenge_method=S256"))
        (is (not (str/includes? location (:verifier claims))))
        (binding [auth/*exchange-code!*
                  (fn [_ code verifier]
                    (reset! exchanged [code verifier])
                    {:id_token (id-token key {:now now
                                              :nonce (:nonce claims)})})
                  auth/*fetch-jwks!* (constantly (public-jwks key))]
          (is (= "operator@example.invalid"
                 (:email (auth/complete-login!
                           config {:code "synthetic-code"
                                   :state (:state claims)
                                   :transaction transaction}))))
          (is (= ["synthetic-code" (:verifier claims)] @exchanged))
          (is (= :invalid-login-state
                 (try
                   (auth/complete-login!
                     config {:code "synthetic-code"
                             :state "wrong-state"
                             :transaction transaction})
                   nil
                   (catch clojure.lang.ExceptionInfo error
                     (:type (ex-data error)))))))))))

(deftest browser-session-is-signed-expiring-and-csrf-bound
  (let [clock (atom 3000000)]
    (binding [auth/*now-ms* #(deref clock)]
      (let [token (auth/new-session
                    config {:subject "synthetic-google-subject"
                            :email "operator@example.invalid"})
            session (auth/session config token)]
        (is (= "operator@example.invalid" (:email session)))
        (is (auth/csrf-valid? session (:csrf session)))
        (is (not (auth/csrf-valid? session "wrong-token")))
        (is (nil? (auth/session config (str token "changed"))))
        (swap! clock + (:session-ttl-ms config))
        (is (nil? (auth/session config token)))))))
