(ns cch.control.mcp-http-test
  (:require [cch.control.mcp :as mcp]
            [cch.control.mcp-http :as mcp-http]
            [clojure.test :refer [deftest is testing]]))

(deftest parse-tokens-reads-token-to-caller-map
  (testing "a JSON object of token -> caller"
    (is (= {"abc" "codex" "def" "claude"}
           (mcp-http/parse-tokens "{\"abc\":\"codex\",\"def\":\"claude\"}"))))
  (testing "blank/malformed/empty yields nil (endpoint stays inert)"
    (is (nil? (mcp-http/parse-tokens "")))
    (is (nil? (mcp-http/parse-tokens "   ")))
    (is (nil? (mcp-http/parse-tokens "{")))
    (is (nil? (mcp-http/parse-tokens "[\"abc\"]")))
    (is (nil? (mcp-http/parse-tokens "{}"))))
  (testing "entries with blank token or caller are dropped"
    (is (= {"abc" "codex"}
           (mcp-http/parse-tokens "{\"abc\":\"codex\",\"\":\"claude\",\"x\":\"\"}")))))

(deftest bearer-token-extraction-is-case-tolerant
  (is (= "tok" (mcp-http/bearer-token {:headers {"authorization" "Bearer tok"}})))
  (is (= "tok" (mcp-http/bearer-token {:headers {"Authorization" "bearer   tok"}})))
  (is (nil? (mcp-http/bearer-token {:headers {"authorization" "Basic tok"}})))
  (is (nil? (mcp-http/bearer-token {:headers {"authorization" "Bearer"}})))
  (is (nil? (mcp-http/bearer-token {:headers {}})))
  (is (nil? (mcp-http/bearer-token {}))))

(deftest resolve-caller-maps-token-to-agent
  (let [tokens {"codex-tok" "codex" "claude-tok" "claude"}]
    (is (= "codex" (mcp-http/resolve-caller tokens {:headers {"authorization" "Bearer codex-tok"}})))
    (is (= "claude" (mcp-http/resolve-caller tokens {:headers {"authorization" "Bearer claude-tok"}})))
    (is (nil? (mcp-http/resolve-caller tokens {:headers {"authorization" "Bearer nope"}})))
    (is (nil? (mcp-http/resolve-caller nil {:headers {"authorization" "Bearer codex-tok"}})))))

(deftest handle-503-when-tokens-not-provisioned
  (with-redefs [mcp-http/load-tokens (constantly nil)]
    (is (= 503 (:status (mcp-http/handle {} (fn [_] {:status 200})))))))

(deftest handle-401-on-missing-or-unknown-token
  (with-redefs [mcp-http/load-tokens (constantly {"good" "codex"})]
    (let [delegate (fn [_] {:status 200})]
      (is (= 401 (:status (mcp-http/handle {} delegate))))
      (is (= 401 (:status (mcp-http/handle {:headers {"authorization" "Bearer bad"}} delegate))))
      (is (= "Bearer" (get-in (mcp-http/handle {} delegate)
                              [:headers "WWW-Authenticate"]))))))

(deftest handle-binds-caller-and-delegates-on-valid-token
  (with-redefs [mcp-http/load-tokens (constantly {"codex-tok" "codex"})]
    (let [seen (atom :unset)
          delegate (fn [_] (reset! seen mcp/*caller-override*) {:status 200 :body "ok"})
          resp (mcp-http/handle {:headers {"authorization" "Bearer codex-tok"}} delegate)]
      (is (= 200 (:status resp)))
      (is (= "codex" @seen) "the caller kind is bound for the tool call")
      (is (nil? mcp/*caller-override*) "the binding does not leak past the request"))))
