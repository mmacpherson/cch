(ns cch.activity-trace-test
  (:require [cch.activity-trace :as trace]
            [cch.scrub :as scrub]
            [cheshire.core :as json]
            [clojure.test :refer [deftest is testing]]))

(defn row [id event payload & [overrides]]
  (merge {:id id :timestamp (format "2026-01-01T00:00:%02d.000" id)
          :session_id "session-a" :agent "claude-code"
          :hook_name "event-log" :event_type event :extra (json/generate-string payload)}
         overrides))

(deftest tool-summaries
  (doseq [[tool input expected]
          [["Bash" {:command "  just test
secret output"} "just test"]
           ["Read" {:file_path "src/example.clj"} "src/example.clj"]
           ["Edit" {:file_path "src/example.clj" :old_string "excluded"} "src/example.clj"]
           ["Write" {:file_path "src/example.clj" :content "excluded"} "src/example.clj"]
           ["Grep" {:pattern "needle"} "needle"]
           ["Glob" {:pattern "**/*.clj"} "**/*.clj"]
           ["mcp__example__query" {:query "excluded"} "mcp__example__query"]
           ["apply_patch" {:patch "*** Begin Patch
*** Update File: a.clj
*** Move to: b.clj
*** Add File: c.clj
*** End Patch"} "a.clj, b.clj, c.clj"]
           ["Bash" (json/generate-string {:command "pwd"}) "pwd"]
           ["Read" nil "Read"]
           ["unknown" nil "unknown"]]]
    (is (= expected (trace/tool-summary tool input))))
  (is (= 160 (count (trace/tool-summary "Bash" {:command (apply str (repeat 200 "a"))})))))

(deftest claude-turn-pairs-tools-and-excludes-output
  (let [[turn] (trace/turns
                 [(row 1 "UserPromptSubmit" {:prompt_id "prompt-a" :prompt "fix it"})
                  (row 2 "PreToolUse" {:tool_use_id "tool-a" :tool_name "Bash" :tool_input {:command "just test"}})
                  (row 3 "PostToolUse" {:tool_use_id "tool-a" :tool_name "Bash" :tool_response "NEVER INCLUDE"})
                  (row 4 "Stop" {:last_assistant_message "done"})])
        [tool] (:steps turn)]
    (is (= "prompt-a" (:turn-key turn)))
    (is (= [{:at "2026-01-01T00:00:01.000" :text "fix it"}] (:prompts turn)))
    (is (= {:at "2026-01-01T00:00:02.000" :kind :tool :summary "just test"
            :tool "Bash" :outcome :ok :duration-ms 1000} tool))
    (is (= "2026-01-01T00:00:04.000" (:ended-at turn)))
    (is (= {:prompt "fix it" :tool-summaries ["just test"] :closing-message "done"}
           (trace/digest-input turn)))
    (is (not (re-find #"NEVER INCLUDE|pending" (pr-str turn))))
    (is (= "fix it
just test
done" (:text (scrub/digest (trace/digest-input turn)))))))

(deftest codex-steering-and-concurrent-tools
  (let [rows [(row 1 "UserPromptSubmit" {:turn_id "turn-a" :prompt "initial"})
              (row 2 "PreToolUse" {:turn_id "turn-a" :tool_use_id "a" :tool_name "Read" :tool_input {:file_path "a"}})
              (row 3 "PreToolUse" {:turn_id "turn-a" :tool_use_id "b" :tool_name "Read" :tool_input {:file_path "b"}})
              (row 4 "UserPromptSubmit" {:turn_id "turn-a" :prompt "steered"})
              (row 5 "PostToolUseFailure" {:turn_id "turn-a" :tool_use_id "b" :duration_ms 42})
              (row 6 "PostToolUse" {:turn_id "turn-a" :tool_use_id "a"})
              (row 7 "Stop" {:turn_id "turn-a"})]
        turns (trace/turns (map #(assoc % :agent "codex") rows))
        [turn] turns]
    (is (= 1 (count turns)))
    (is (= "turn-a" (:turn-key turn)))
    (is (= ["initial" "steered"] (mapv :text (:prompts turn))))
    (is (= [:ok :failed] (mapv :outcome (:steps turn))))
    (is (= [4000 42] (mapv :duration-ms (:steps turn))))))

(deftest pending-and-unknown
  (let [pre (row 2 "PreToolUse" {:tool_use_id "a" :tool_name "Read"})
        rows [(row 1 "UserPromptSubmit" {:prompt "hello"}) pre]]
    (is (= :pending (-> (trace/turns rows) first :steps first :outcome)))
    (is (= :unknown (-> (trace/turns (conj rows (row 3 "Stop" {}))) first :steps first :outcome)))
    (is (= :unknown (-> (trace/turns (conj rows (row 3 "UserPromptSubmit" {:prompt "next"}))) first :steps first :outcome)))))

(deftest structural-events-and-decisions
  (let [rows [(row 1 "SessionStart" {} {:cwd "/tmp/project"})
              (row 2 "UserPromptSubmit" {:prompt_id "p" :prompt "hello"})
              (row 3 "PreToolUse" {:prompt_id "p"} {:hook_name "command-guard" :decision "deny" :reason "blocked"})
              (row 4 "PreToolUse" {} {:hook_name "scope-lock" :decision "ask" :reason "confirm"})
              (row 5 "PreToolUse" {} {:hook_name "protect-files" :decision "allow"})
              (row 6 "PermissionRequest" {:tool_name "Bash" :tool_input {:command "deploy"}})
              (row 7 "PreCompact" {:trigger "auto"})
              (row 8 "PostCompact" {})
              (row 9 "SubagentStart" {:agent_type "worker"})
              (row 10 "SubagentStop" {:agent_id "child"})
              (row 11 "Notification" {:message "waiting
private second line"})
              (row 12 "CwdChanged" {:cwd "/tmp/other"})
              (row 13 "Stop" {})
              (row 14 "Notification" {:message "after stop"})]
        turns (trace/turns rows)
        [leading main trailing] turns]
    (is (= 3 (count turns)))
    (is (empty? (:prompts leading)))
    (is (= [:cwd :notification] (mapv :kind (:steps leading))))
    (is (= [:hook-decision :hook-decision :permission :compaction :compaction :subagent :subagent :notification :cwd]
           (mapv :kind (:steps main))))
    (is (= [:denied :asked :asked] (mapv :outcome (take 3 (:steps main)))))
    (is (= "waiting" (:summary (nth (:steps main) 7))))
    (is (= "after stop" (-> trailing :steps first :summary)))))

(deftest malformed-partial-and-missing-data
  (is (= [] (trace/turns [])))
  (doseq [extra [nil "bad json" "[]" "42" "null"]]
    (let [[turn] (trace/turns [(row 1 "PreToolUse" {} {:extra extra :tool_name "Read"})])]
      (is (= "Read" (-> turn :steps first :summary)))
      (is (string? (:turn-key turn)))))
  (testing "a completion outside the query window remains visible"
    (let [[turn] (trace/turns [(row 1 "PostToolUse" {:tool_name "Read" :tool_input {:file_path "a"}})])]
      (is (= :ok (-> turn :steps first :outcome)))
      (is (nil? (-> turn :steps first :duration-ms)))))
  (testing "late completion goes to the original turn"
    (let [[a b] (trace/turns [(row 1 "UserPromptSubmit" {:prompt_id "a"})
                             (row 2 "PreToolUse" {:prompt_id "a" :tool_use_id "tool" :tool_name "Read"})
                             (row 3 "UserPromptSubmit" {:prompt_id "b"})
                             (row 4 "PostToolUse" {:prompt_id "a" :tool_use_id "tool"})])]
      (is (= :ok (-> a :steps first :outcome)))
      (is (= "b" (:turn-key b))))))

(deftest permission-denial-and-codex-exit-status
  (let [[turn] (trace/turns
                 [(row 1 "UserPromptSubmit" {:turn_id "a"})
                  (row 2 "PermissionDenied" {:tool_name "Bash" :tool_input {:command "deploy"}})
                  (row 3 "PreToolUse" {:tool_use_id "x" :tool_name "exec_command" :tool_input {:cmd "just test"}})
                  (row 4 "PostToolUse" {:tool_use_id "x" :tool_response {:exit_code 1 :output "excluded"}})])]
    (is (= [:permission :tool] (mapv :kind (:steps turn))))
    (is (= [:denied :failed] (mapv :outcome (:steps turn))))
    (is (= "just test" (-> turn :steps second :summary)))))

(deftest repeated-missing-ids-and-session-isolation
  (let [turns (trace/turns
                [(row 1 "UserPromptSubmit" {:prompt "one"})
                 (row 2 "PreToolUse" {:tool_name "Read"})
                 (row 3 "UserPromptSubmit" {:prompt "other"} {:session_id "session-b"})
                 (row 4 "Stop" {})
                 (row 5 "UserPromptSubmit" {:prompt "two"})])]
    (is (= 3 (count turns)))
    (is (= 3 (count (set (map :turn-key turns)))))
    (is (nil? (:ended-at (second turns))))
    (is (= :unknown (-> turns first :steps first :outcome)))))

(deftest duplicate-dispatchers-do-not-multiply-tools
  (let [rows [(row 1 "UserPromptSubmit" {:turn_id "t" :prompt "hello"})
              (row 2 "PreToolUse" {:turn_id "t" :tool_use_id "a" :tool_name "Bash" :tool_input {:command "pwd"}})
              (row 3 "PreToolUse" {:turn_id "t" :tool_use_id "a" :tool_name "Bash" :tool_input {:command "pwd"}})
              (row 4 "PostToolUse" {:turn_id "t" :tool_use_id "a"})
              (row 5 "PostToolUse" {:turn_id "t" :tool_use_id "a" :duration_ms 100})
              (row 6 "PreToolUse" {:turn_id "t" :tool_use_id "b" :tool_name "Bash" :tool_input {:command "pwd"}})
              (row 7 "Stop" {:turn_id "t"})]
        [turn] (trace/turns rows)]
    (is (= ["pwd" "pwd"] (mapv :summary (:steps turn))))
    (is (= [:ok :unknown] (mapv :outcome (:steps turn))))))

(deftest heredoc-summaries
  (is (= "python3 - <<'EOF' (heredoc) import sys"
         (trace/tool-summary "Bash" {:command "python3 - <<'EOF'\n\nimport sys\nprint(sys.version)\nEOF"})))
  (is (= 160 (count (trace/tool-summary "Bash" {:command (str "cat <<BODY\n" (apply str (repeat 200 "x")) "\nBODY")})))))

(deftest late-completion-without-turn-id
  (let [[first-turn second-turn]
        (trace/turns [(row 1 "UserPromptSubmit" {:prompt_id "a"})
                      (row 2 "PreToolUse" {:tool_use_id "tool" :tool_name "Read"})
                      (row 3 "UserPromptSubmit" {:prompt_id "b"})
                      (row 4 "PostToolUse" {:tool_use_id "tool"})])]
    (is (= :ok (-> first-turn :steps first :outcome)))
    (is (= 2000 (-> first-turn :steps first :duration-ms)))
    (is (empty? (:steps second-turn)))))

(deftest denied-tool-is-no-longer-pending
  (let [[turn] (trace/turns [(row 1 "PreToolUse" {:tool_use_id "x" :tool_name "Bash"})
                            (row 2 "PermissionDenied" {:tool_use_id "x" :tool_name "Bash"})])]
    (is (= [:denied :denied] (mapv :outcome (:steps turn))))))
