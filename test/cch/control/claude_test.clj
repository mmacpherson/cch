(ns cch.control.claude-test
  (:require [babashka.fs :as fs]
            [cch.control.claude :as claude]
            [cch.control.store :as store]
            [cheshire.core :as json]
            [clojure.test :refer [deftest is testing]])
  (:import [java.io BufferedReader]
           [java.net StandardProtocolFamily UnixDomainSocketAddress]
           [java.nio.channels Channels ServerSocketChannel]
           [java.nio.charset StandardCharsets]))

(deftest hook-registration-requires-the-native-inbox
  (testing "supported payload and environment register without a launcher"
    (let [captured (atom nil)]
      (with-redefs [store/upsert-claude! #(do (reset! captured %) "claude:synthetic")]
        (is (= "claude:synthetic"
               (claude/register-from-hook!
                 {:session_id "synthetic" :cwd "/synthetic/project"
                  :transcript_path "/synthetic/transcript.jsonl"}
                 {"CLAUDE_CODE_MESSAGING_SOCKET" "/tmp/321.sock"
                  "CLAUDE_CODE_MESSAGING_TOKEN" "token"})))
        (is (= 321 (:pid @captured)))
        (is (= "/synthetic/transcript.jsonl" (:transcript-path @captured))))))
  (testing "older/disabled Claude versions fail clearly"
    (is (= :unsupported-claude-version
           (try
             (claude/register-from-hook! {:session_id "synthetic"} {})
             (catch clojure.lang.ExceptionInfo e (:type (ex-data e))))))))

(deftest discovers-and-caches-exact-remote-control-session-url
  (let [dir (str (fs/create-temp-dir {:prefix "cch-claude-transcript-"}))
        transcript (str dir "/synthetic.jsonl")
        socket (str dir "/123.sock")
        cached (atom nil)]
    (try
      (spit transcript
            (str (json/generate-string
                   {:type "user" :message {:content "Ignore https://evil.invalid"}})
                 "\n"
                 (json/generate-string
                   {:type "system" :subtype "bridge_status"
                    :url "https://claude.ai/code/session_synthetic123"})
                 "\n"))
      (spit socket "")
      (with-redefs [store/claude-sessions
                    (constantly [{:route_id "claude:session-1"
                                  :native_id "session-1"
                                  :socket_path socket
                                  :transcript_path transcript}])
                    store/set-claude-native-url!
                    (fn [_ value] (reset! cached value))
                    claude/native-sessions
                    (constantly [{:sessionId "session-1" :status "idle"}])]
        (let [session (first (claude/sessions))]
          (is (= "https://claude.ai/code/session_synthetic123"
                 (:native-url session)))
          (is (= (:native-url session) @cached))))
      (finally
        (fs/delete-tree dir)))))

(deftest send-emits-only-auth-and-user-frames
  (let [dir (str (fs/create-temp-dir {:prefix "cch-claude-uds-"}))
        socket-path (str dir "/inbox.sock")
        server (ServerSocketChannel/open StandardProtocolFamily/UNIX)]
    (try
      (.bind server (UnixDomainSocketAddress/of socket-path))
      (let [received
            (future
              (with-open [channel (.accept server)
                          reader (BufferedReader.
                                   (Channels/newReader channel
                                                       (.newDecoder StandardCharsets/UTF_8)
                                                       -1))]
                [(json/parse-string (.readLine reader) true)
                 (json/parse-string (.readLine reader) true)]))]
        (with-redefs [store/claude-session
                      (constantly {:native_id "session-1"
                                   :socket_path socket-path
                                   :auth_token "synthetic-token"})]
          (claude/send! {:route-id "claude:session-1"
                         :message "Synthetic POC ping"
                         :message-id "message-1"
                         :source "codex:session-2"}))
        (let [result (deref received 3000 ::timeout)
              [auth frame] (when-not (= ::timeout result) result)]
          (is (not= ::timeout result))
          (is (= {:type "auth" :token "synthetic-token"} auth))
          (is (= "user" (:type frame)))
          (is (= "user" (get-in frame [:message :role])))
          (is (re-find #"codex:session-2" (get-in frame [:message :content])))
          (is (nil? (:action frame)) "no native control frame can be emitted")))
      (finally
        (.close server)
        (fs/delete-tree dir)))))
