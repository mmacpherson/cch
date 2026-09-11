(ns cch.control.mcp-tokens-test
  (:require [babashka.fs :as fs]
            [cch.control.mcp-http :as mcp-http]
            [cch.control.mcp-tokens :as tokens]
            [clojure.test :refer [deftest is testing]]))

(defn- with-temp-path [f]
  (let [dir (str (fs/create-temp-dir {:prefix "mcp-tokens-test-"}))
        path (str dir "/mcp-tokens.json")]
    (try (f path) (finally (fs/delete-tree dir)))))

(deftest mint-token-is-random-urlsafe-and-unique
  (let [ts (repeatedly 50 tokens/mint-token)]
    (is (every? #(re-matches #"[A-Za-z0-9_-]+" %) ts) "url-safe, unpadded")
    (is (every? #(>= (count %) 40) ts) "256-bit tokens are long")
    (is (= 50 (count (set ts))) "no collisions")))

(deftest caller->token-and-token-for-invert
  (let [t {"aaa" "codex" "bbb" "claude"}]
    (is (= {"codex" "aaa" "claude" "bbb"} (tokens/caller->token t)))
    (is (= "aaa" (tokens/token-for t "codex")))
    (is (= "bbb" (tokens/token-for t "claude")))
    (is (nil? (tokens/token-for t "gemini")))))

(deftest ensure-tokens!-mints-one-per-kind-and-is-idempotent
  (with-temp-path
    (fn [path]
      (let [t1 (tokens/ensure-tokens! path)]
        (testing "a token per caller kind, readable back through the loader"
          (is (= #{"codex" "claude"} (set (vals t1))))
          (is (= t1 (mcp-http/parse-tokens (slurp path)))))
        (testing "owner-only permissions"
          (when (fs/exists? path)
            (is (= "rw-------"
                   (fs/posix->str (fs/posix-file-permissions path))))))
        (testing "second call is stable — same tokens, no rewrite"
          (let [before (fs/last-modified-time path)
                t2 (tokens/ensure-tokens! path)]
            (is (= t1 t2))
            (is (= before (fs/last-modified-time path)))))))))

(deftest ensure-tokens!-tops-up-a-missing-kind
  (with-temp-path
    (fn [path]
      ;; Seed a file that only has codex; ensure! should add claude, keep codex.
      (spit path "{\"existing-codex\":\"codex\"}")
      (let [t (tokens/ensure-tokens! path)]
        (is (= "codex" (get t "existing-codex")) "existing token preserved")
        (is (= #{"codex" "claude"} (set (vals t))))
        (is (some? (tokens/token-for t "claude")))))))
