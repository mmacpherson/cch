(ns cch.control.web-auth-test
  (:require [cch.control.web-auth :as auth]
            [clojure.test :refer [deftest is]])
  (:import [com.nimbusds.jose JWSAlgorithm JWSHeader$Builder]
           [com.nimbusds.jose.crypto RSASSASigner]
           [com.nimbusds.jose.jwk JWKSet]
           [com.nimbusds.jose.jwk.gen RSAKeyGenerator]
           [com.nimbusds.jwt JWTClaimsSet$Builder SignedJWT]
           [java.util Date]))

(def config
  {:origin "https://control.invalid"
   :issuer "https://synthetic.cloudflareaccess.com"
   :audience "synthetic-access-audience"
   :allowed-emails #{"operator@example.invalid"}
   :session-secret "synthetic-session-secret-with-more-than-32-characters"
   :listen-host "127.0.0.1"
   :listen-port 8788
   :jwks-url "https://synthetic.cloudflareaccess.com/cdn-cgi/access/certs"})

(defn- signing-key [key-id]
  (-> (RSAKeyGenerator. 2048)
      (.keyID key-id)
      .generate))

(defn- access-token
  [key {:keys [now audience email issuer subject expires-at]
        :or {audience "synthetic-access-audience"
             email "operator@example.invalid"
             issuer "https://synthetic.cloudflareaccess.com"
             subject "synthetic-access-subject"}}]
  (let [claims (-> (JWTClaimsSet$Builder.)
                   (.issuer issuer)
                   (.subject subject)
                   (.audience audience)
                   (.issueTime (Date. now))
                   (.expirationTime (Date. (or expires-at (+ now 300000))))
                   (.claim "email" email)
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
             "CCH_CONTROL_CLOUDFLARE_ISSUER"
             "https://synthetic.cloudflareaccess.com/"
             "CCH_CONTROL_CLOUDFLARE_AUDIENCE" "synthetic-access-audience"
             "CCH_CONTROL_WEB_ALLOWED_EMAILS"
             " Operator@Example.invalid, second@example.invalid "
             "CCH_CONTROL_WEB_SESSION_SECRET"
             "synthetic-session-secret-with-more-than-32-characters"
             "CCH_CONTROL_WEB_HOST" "0.0.0.0"
             "CCH_CONTROL_WEB_PORT" "9000"}
        parsed (auth/config-from-env env)]
    (is (= #{"operator@example.invalid" "second@example.invalid"}
           (:allowed-emails parsed)))
    (is (= "https://control.invalid" (:origin parsed)))
    (is (= "https://synthetic.cloudflareaccess.com" (:issuer parsed)))
    (is (= "0.0.0.0" (:listen-host parsed)))
    (is (= 9000 (:listen-port parsed))))
  (doseq [env [{"CCH_CONTROL_WEB_ORIGIN" "https://control.invalid"}
               {"CCH_CONTROL_WEB_ORIGIN" "https://control.invalid"
                "CCH_CONTROL_CLOUDFLARE_ISSUER" "https://issuer.invalid"
                "CCH_CONTROL_CLOUDFLARE_AUDIENCE" "synthetic-access-audience"
                "CCH_CONTROL_WEB_ALLOWED_EMAILS" "operator@example.invalid"
                "CCH_CONTROL_WEB_SESSION_SECRET" (apply str (repeat 40 "x"))}
               {"CCH_CONTROL_WEB_ORIGIN" "https://control.invalid"
                "CCH_CONTROL_CLOUDFLARE_ISSUER"
                "https://synthetic.cloudflareaccess.com"
                "CCH_CONTROL_CLOUDFLARE_AUDIENCE" "synthetic-access-audience"
                "CCH_CONTROL_WEB_ALLOWED_EMAILS" "*.example.invalid"
                "CCH_CONTROL_WEB_SESSION_SECRET" (apply str (repeat 40 "x"))}
               {"CCH_CONTROL_WEB_ORIGIN" "https://control.invalid"
                "CCH_CONTROL_CLOUDFLARE_ISSUER"
                "https://synthetic.cloudflareaccess.com"
                "CCH_CONTROL_CLOUDFLARE_AUDIENCE" "synthetic-access-audience"
                "CCH_CONTROL_WEB_ALLOWED_EMAILS" "operator@example.invalid"
                "CCH_CONTROL_WEB_SESSION_SECRET" (apply str (repeat 40 "x"))
                "CCH_CONTROL_WEB_PORT" "70000"}]]
    (is (= :invalid-web-config
           (try (auth/config-from-env env) nil
                (catch clojure.lang.ExceptionInfo error
                  (:type (ex-data error))))))))

(deftest access-token-verification-checks-signature-claims-expiry-and-allowlist
  (let [now 1000000
        key (signing-key "synthetic-key")
        other-key (signing-key "other-key")
        jwks (public-jwks key)]
    (binding [auth/*now-ms* (constantly now)]
      (is (= {:subject "synthetic-access-subject"
              :email "operator@example.invalid"
              :issued-at now
              :expires-at (+ now 300000)}
             (auth/verify-access-token
               config (access-token key {:now now}) jwks)))
      (doseq [[token expected]
              [[(access-token key {:now now :audience "different-audience"})
                :invalid-access-token]
               [(access-token key {:now now :issuer "https://other.invalid"})
                :invalid-access-token]
               [(access-token key {:now now :email "blocked@example.invalid"})
                :identity-not-allowed]
               [(access-token key {:now now :expires-at now})
                :invalid-access-token]
               [(access-token other-key {:now now})
                :access-key-unavailable]]]
        (is (= expected
               (try (auth/verify-access-token config token jwks) nil
                    (catch clojure.lang.ExceptionInfo error
                      (:type (ex-data error))))))))))

(deftest request-authentication-requires-access-header-and-caches-keys
  (let [now 2000000
        key (signing-key "cached-key")
        fetches (atom 0)
        unique-config (assoc config
                             :issuer "https://cache-test.cloudflareaccess.com"
                             :jwks-url (str "https://cache-test.cloudflareaccess.com/"
                                            "cdn-cgi/access/certs"))
        token (access-token key {:now now
                                 :issuer (:issuer unique-config)})]
    (binding [auth/*now-ms* (constantly now)
              auth/*fetch-jwks!* (fn [_]
                                   (swap! fetches inc)
                                   (public-jwks key))]
      (is (= :access-required
             (try (auth/authenticate! unique-config {:headers {}}) nil
                  (catch clojure.lang.ExceptionInfo error
                    (:type (ex-data error))))))
      (dotimes [_ 2]
        (is (= "operator@example.invalid"
               (:email (auth/authenticate!
                         unique-config
                         {:headers {"cf-access-jwt-assertion" token}})))))
      (is (= 1 @fetches)))))

(deftest csrf-is-signed-and-bound-to-the-access-session
  (let [identity {:subject "synthetic-access-subject"
                  :email "operator@example.invalid"
                  :issued-at 3000000
                  :expires-at 3300000}
        other (assoc identity :expires-at 3400000)
        token (auth/csrf-token config identity)]
    (is (auth/csrf-valid? config identity token))
    (is (not (auth/csrf-valid? config identity (str token "changed"))))
    (is (not (auth/csrf-valid? config other token)))
    (is (= "https://control.invalid/cdn-cgi/access/logout"
           (auth/logout-location config)))))
