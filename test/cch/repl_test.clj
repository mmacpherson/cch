(ns cch.repl-test
  "Smoke tests for the optional nREPL server wired into cch.server.

  Spins up a real nREPL on a free port via cch.server/start! (which
  also boots the HTTP dispatcher + log writer), evals a tiny expression
  via raw bencode-over-TCP, and asserts the value comes back.

  We hit the protocol directly so this test exercises the same wire
  format that bin/cch-eval uses."
  (:require [babashka.fs :as fs]
            [bencode.core :as b]
            [cch.log :as log]
            [cch.server :as server]
            [clojure.test :refer [deftest is testing]])
  (:import (java.net Socket ServerSocket)))

(defn- free-port []
  (with-open [s (ServerSocket. 0)]
    (.getLocalPort s)))

(defn- eval-once
  "Send {op eval code id} over a fresh TCP socket and read until status
  contains 'done'. Returns the decoded :value string or nil."
  [port code]
  (with-open [sock (Socket. "127.0.0.1" (int port))]
    (let [out (.getOutputStream sock)
          in  (java.io.PushbackInputStream. (.getInputStream sock))]
      (b/write-bencode out {"op" "eval" "code" code "id" "t1"})
      (.flush out)
      (loop [last-val nil]
        (let [msg    (b/read-bencode in)
              decode #(when % (String. ^bytes % "UTF-8"))
              status (when-let [s (get msg "status")] (set (map decode s)))
              v      (decode (get msg "value"))]
          (cond
            (and status (status "done")) (or v last-val)
            :else                         (recur (or v last-val))))))))

(deftest test-nrepl-eval-roundtrip
  (testing "with --nrepl <port> set, an nREPL accepts eval ops"
    (let [http  (free-port)
          nport (free-port)
          tmp   (str (fs/create-temp-dir {:prefix "repl-test-db-"}))]
      (with-redefs [log/db-path (fn [] (str tmp "/test.db"))]
        (let [{:keys [stop]} (server/start! {:port http :host "127.0.0.1"
                                              :nrepl-port nport})]
          (try
            (is (= "3" (eval-once nport "(+ 1 2)")))
            (testing "in-process atoms are inspectable"
              (let [v (eval-once nport "(count @cch.events/subscribers)")]
                (is (re-matches #"\d+" v))))
            (finally
              (stop)
              (fs/delete-tree tmp)))))
      (testing ".nrepl-port file is cleaned up on shutdown"
        (is (not (fs/exists? ".nrepl-port")))))))

(deftest test-nrepl-default-off
  (testing "starting without --nrepl leaves the default port unbound"
    (let [http (free-port)
          tmp  (str (fs/create-temp-dir {:prefix "repl-test-off-db-"}))]
      (with-redefs [log/db-path (fn [] (str tmp "/test.db"))]
        (let [{:keys [stop nrepl]} (server/start! {:port http :host "127.0.0.1"})]
          (try
            (is (nil? nrepl) "no nREPL server when port is unset")
            (finally
              (stop)
              (fs/delete-tree tmp))))))))
