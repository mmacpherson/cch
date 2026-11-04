(ns cch.scrub-test
  (:require [cch.scrub :as scrub]
            [clojure.string :as str]
            [clojure.test :refer [deftest is]]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]))

(deftest redaction-kinds
  (doseq [[kind value]
          [[:token (str "sk-" "exampleCredential1234")]
           [:token (str "ghp" "_exampleCredential1234")]
           [:token (str "xoxb" "-1234567890-abcdefgh")]
           [:token (str "AKIA" (apply str (repeat 16 "X"))) ]
           [:auth (str "Bearer " "exampleCredential")]
           [:auth (str "Basic " "ZXhhbXBsZTpleGFtcGxl")]
           [:credential (str "API_KEY=" "sample-value")]
           [:credential (str "{\"password\":\"" "sample-value\"}")]
           [:jwt (str "eyJhbGci" ".eyJzdWIi" ".c2lnbmF0dXJl")]
           [:pem (str "-----BEGIN " "PRIVATE KEY-----\nexample\n-----END " "PRIVATE KEY-----")]
           [:userinfo (str "https://" "example:sample@")]
           [:email (str "example" "@example.invalid")]
           [:phone (str "+1 " "202-555-0123")]
           [:phone (str "+1" "2025550123")]
           [:ip (str "192.0." "2.1")]
           [:ip (str "2001:" "db8::1")]
           [:ip (str "::" "1") ]
           [:hostname "https://service.example.invalid"]]]
    (let [{:keys [text counts]} (scrub/scrub value)]
      (is (not (str/includes? text value)) (name kind))
      (is (pos? (get counts kind 0)) (name kind)))))

(deftest options-and-digest
  (is (= "~/work" (:text (scrub/scrub (str "/home/" "example/work")))))
  (is (= "runner-a" (:text (scrub/scrub "service.example.invalid" {:known-hosts {"service.example.invalid" "runner-a"}}))))
  (is (= {:text "[redacted:custom]" :counts {:custom 1}}
         (scrub/scrub "private-marker" {:extra-patterns [{:kind :custom :pattern "private-marker"}]})))
  (is (= "abcd" (:text (scrub/scrub "abcdef" {:max-chars 4})) ))
  (is (= "hello\nread file\ndone"
         (:text (scrub/digest {:prompt "hello" :tool-summaries ["read file\nraw content"]
                               :closing-message "done" :tool-output "forbidden" :transcript-path "forbidden"}))))
  (is (= "" (:text (scrub/scrub nil))))
  (is (thrown? clojure.lang.ExceptionInfo (scrub/scrub "text" {:max-chars -1}))))

(defspec generated-command-secrets 300
  (prop/for-all [body (gen/fmap #(apply str %) (gen/vector (gen/elements (vec "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_-")) 8 80))]
    (let [secrets [(str "sk-" body) (str "ghp_" body) (str "xoxb-" body)
                   (str "sample" body)]
          commands [(str "curl -H 'Authorization: Bearer " (first secrets) "' https://example.invalid")
                    (str "git -c token=" (second secrets) " fetch")
                    (str "notify --key " (nth secrets 2))
                    (str "env PASSWORD='" (nth secrets 3) "' deploy")]]
      (every? true? (map (fn [command secret]
                          (not (str/includes? (:text (scrub/scrub command)) secret))) commands secrets)))))

(deftest entropy-and-readability
  (is (= "run tests and fix the parser" (:text (scrub/scrub "run tests and fix the parser"))))
  (let [secret (apply str (map char (range 65 91)))]
    (is (= {:text "[redacted:entropy]" :counts {:entropy 1}} (scrub/scrub secret)))))

(defspec generated-private-context 200
  (prop/for-all [body (gen/fmap #(apply str %) (gen/vector gen/char-alpha 8 24))]
    (let [email (str body "@example.invalid")
          home (str "/Users/" body)
          host (str body ".example.invalid")
          text (:text (scrub/scrub (str "ssh " email " 'cd " home "/work; curl https://" host "'")))]
      (every? #(not (str/includes? text %)) [email home host]))))

(deftest review-regressions
  (doseq [prefix ["glpat-" "AIza" "sk_live_" "rk_live_" "pk_live_" "npm_" "hf_" "xapp-" "ya29." "sk-ant-"]]
    (let [secret (str prefix "example1234")]
      (is (= 1 (get-in (scrub/scrub secret) [:counts :token])))))
  (is (= "mysql --password [redacted:credential]"
         (:text (scrub/scrub "mysql --password sample-value"))))
  (is (= "API_KEY=[redacted:credential]"
         (:text (scrub/scrub "API_KEY=sample-value"))))
  (is (= "ssh runner-a" (:text (scrub/scrub "ssh machine-a"
                                  {:known-hosts {"machine-a" "runner-a"}})))))
(defspec benign-developer-text 200
  (prop/for-all [s (gen/elements ["Edit src/cch/scrub.clj and README.md" "cch.control.core"
                                "deploy at 12:34:56" "a:b:c" "version 1.2.3" "just test"
                                "localhost:8888" "1234567890abcdef1234567890abcdef12345678"
                                "12345678-1234-1234-1234-123456789abc"])]
    (= s (:text (scrub/scrub s)))))

(defspec generated-credential-contexts 200
  (prop/for-all [body (gen/fmap #(apply str %) (gen/vector gen/char-alphanumeric 8 40))]
    (let [secret (str "sample-" body)]
      (every? #(not (str/includes? (:text (scrub/scrub %)) secret))
              [(str "curl -H 'X-Api-Key: " secret "' https://example.invalid")
               (str "env SECRET=" secret " deploy")
               (str "{\"password\":\"" secret "\"}")
               (str "curl 'https://example.invalid?token=" secret "'")
               (str "mysql --password " secret)]))))

(deftest truncation-keeps-placeholders-whole
  (doseq [n (range 1 16)]
    (is (= "" (:text (scrub/scrub (str "sk-" "example1234") {:max-chars n}))))))

(deftest host-identity-and-key-boundaries
  (is (= "ssh runner-a then ssh runner-b"
         (:text (scrub/scrub "ssh machine-a then ssh machine-b"
                            {:known-hosts {"machine-a" "runner-a" "machine-b" "runner-b"}}))))
  (is (= "scp f [redacted:hostname]:/x"
         (:text (scrub/scrub "scp f host.internal:/x"))))
  (is (= "tokens: 42 author: example" (:text (scrub/scrub "tokens: 42 author: example")))))
