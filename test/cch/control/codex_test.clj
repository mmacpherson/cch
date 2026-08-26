(ns cch.control.codex-test
  (:require [cch.control.codex :as codex]
            [clojure.test :refer [deftest is]]))

(def ^:private fake-app-server
  ["bash" "-c"
   (str "while IFS= read -r line; do "
        "case \"$line\" in "
        "*'\"method\":\"initialize\"'*) echo '{\"id\":1,\"result\":{}}' ;; "
        "*'\"method\":\"thread/list\"'*) echo '{\"id\":2,\"result\":{\"data\":[{\"id\":\"thread-1\",\"cwd\":\"/synthetic/project\",\"status\":{\"type\":\"idle\"},\"canAcceptDirectInput\":true,\"cliVersion\":\"0.149.0\",\"source\":\"cli\"}]}}' ;; "
        "*'\"method\":\"thread/queue/add\"'*) echo '{\"id\":3,\"result\":{\"queuedSubmission\":{}}}' ;; "
        "esac; done")])

(deftest discovers-and-queues-through-app-server-json-rpc
  (binding [codex/*proxy-command* fake-app-server]
    (let [sessions (codex/sessions)]
      (is (= ["codex:thread-1"] (mapv :id sessions)))
      (is (true? (:available (first sessions)))))
    (let [result (codex/send! {:route-id "codex:thread-1"
                               :message "Synthetic POC ping"
                               :message-id "message-1"
                               :source "claude:session-1"})]
      (is (= "codex-app-server" (:transport result)))
      (is (= "submitted" (:status result))))))

(deftest unavailable-daemon-has-actionable-error
  (binding [codex/*proxy-command* ["bash" "-c" "echo native-daemon-missing >&2; exit 1"]]
    (let [message (try (codex/sessions) nil
                       (catch clojure.lang.ExceptionInfo e (.getMessage e)))]
      (is (re-find #"shared app-server daemon is unavailable" message))
      (is (re-find #"codex remote-control start" message)))))
