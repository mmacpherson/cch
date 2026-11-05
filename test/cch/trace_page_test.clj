(ns cch.trace-page-test
  (:require [cch.trace-page :as page]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [hiccup2.core :as hic])
  (:import [java.time ZoneId]))

(def ^:private utc (ZoneId/of "UTC"))

(defn- ms [ts] (page/event-ts->ms ts))

(deftest event-timestamps-are-utc
  (is (= 0 (ms "1970-01-01T00:00:00.000")))
  (is (= 1500 (ms "1970-01-01T00:00:01.500")))
  (is (nil? (ms "not a time")))
  (is (nil? (ms nil))))

(deftest routes-follow-the-agent
  (is (= "codex:t1" (page/route-id "codex" "t1")))
  (is (= "claude:s1" (page/route-id "claude-code" "s1"))))

(deftest message-steps-name-the-peer-without-a-body
  (let [route "claude:me"
        out (page/message-step route {:message_id "m1" :source route :target "codex:peer"
                                      :status "submitted" :created_at 10 :seq 3})
        in (page/message-step route {:message_id "m2" :source "operator" :target route
                                     :status "delivered" :created_at 20 :seq nil})]
    (is (str/starts-with? (:summary out) "→ "))
    (is (str/includes? (:summary out) "m1 · seq 3"))
    (is (= :ok (:outcome out)))
    (is (str/starts-with? (:summary in) "← operator · m2"))
    (is (= :failed (:outcome (page/message-step route {:message_id "m3" :source route
                                                       :target "x" :status "failed"
                                                       :created_at 1}))))))

(deftest messages-land-in-the-turn-running-when-sent
  (let [turns [{:turn-key "a" :started-at "2026-01-01T00:00:00.000"
                :steps [{:at "2026-01-01T00:00:05.000" :kind :tool :summary "ls"}]}
               {:turn-key "b" :started-at "2026-01-01T00:01:00.000" :steps []}]
        msg (fn [ts id] {:at-ms (ms ts) :kind :message :summary id})
        placed (page/place-messages
                 turns
                 [(msg "2026-01-01T00:00:03.000" "in-a")
                  (msg "2026-01-01T00:02:00.000" "in-b")
                  (msg "2025-12-31T23:59:00.000" "early")])]
    (testing "a message before every turn gets its own leading turn"
      (is (= "messages-before-first-turn" (:turn-key (first placed))))
      (is (= ["early"] (map :summary (:steps (first placed))))))
    (testing "messages sort among the turn's own steps by time"
      (is (= ["in-a" "ls"] (map :summary (:steps (second placed))))))
    (is (= ["in-b"] (map :summary (:steps (nth placed 2)))))))

(deftest page-escapes-prompt-and-reply-text
  (let [html (str (hic/html
                    (page/page-body
                      {:agent "claude-code" :session-id "s1" :cwd "/tmp/x"
                       :zone utc :now-ms 0 :messages []
                       :turns [{:turn-key "t" :started-at "2026-01-01T00:00:00.000"
                                :ended-at "2026-01-01T00:00:09.000"
                                :prompts [{:at "2026-01-01T00:00:00.000"
                                           :text "<script>alert(1)</script>"}]
                                :steps [{:at "2026-01-01T00:00:01.000" :kind :tool
                                         :summary "just test" :outcome :ok
                                         :duration-ms 1200}]
                                :closing-message "done <b>bold</b>"}]})))]
    (is (not (str/includes? html "<script>alert")))
    (is (str/includes? html "&lt;script&gt;"))
    (is (str/includes? html "just test"))
    (is (str/includes? html "1 s"))
    (is (str/includes? html "9 s") "turn duration")))

(deftest running-turns-are-marked
  (let [html (str (hic/html
                    (page/page-body
                      {:agent "codex" :session-id "t1" :zone utc
                       :now-ms (ms "2026-01-01T00:00:30.000") :messages []
                       :turns [{:turn-key "t" :started-at "2026-01-01T00:00:00.000"
                                :ended-at nil :prompts [{:text "go"}] :steps []}]})))]
    (is (str/includes? html "running"))
    (is (str/includes? html "started 30 s ago"))))

(deftest long-text-collapses
  (let [long (apply str (repeat 700 "x"))
        html (str (hic/html
                    (page/page-body
                      {:agent "codex" :session-id "t1" :zone utc :now-ms 0 :messages []
                       :turns [{:turn-key "t" :started-at "2026-01-01T00:00:00.000"
                                :ended-at "2026-01-01T00:00:01.000"
                                :prompts [{:text long}] :steps []}]})))]
    (is (str/includes? html "<details"))))

(deftest index-links-each-session
  (let [html (str (hic/html
                    (page/index-body
                      {:zone utc
                       :sessions [{:session_id "s1" :agent "codex" :cwd "/a/b/proj"
                                   :timestamp "2026-01-01T00:00:00.000"}]})))]
    (is (str/includes? html "/trace?session=s1"))
    (is (str/includes? html "proj"))))

(deftest display-names-win-over-mnemonics
  (let [names {"claude:s1" {:name "homelab"} "codex:peer" {:name "reviewer"}}]
    (is (re-matches #"homelab · [a-z]+-[a-z]+-[0-9a-f]{4}" (page/session-label "claude-code" "s1" names)))
    (is (= "homelab" (page/session-name "claude-code" "s1" names)))
    (is (re-matches #"[a-z]+-[a-z]+-[0-9a-f]{4}" (page/session-label "codex" "unknown" names)))
    (is (str/includes? (:summary (page/message-step "claude:s1"
                                                    {:message_id "m" :source "claude:s1"
                                                     :target "codex:peer" :status "submitted"
                                                     :created_at 1}
                                                    names))
                       "reviewer · "))))

(deftest early-messages-start-at-the-earliest
  (let [placed (page/place-messages
                 [{:turn-key "a" :started-at "2026-01-01T01:00:00.000" :steps []}]
                 ;; deliveries arrive newest first
                 [{:at-ms (ms "2026-01-01T00:30:00.000") :kind :message :summary "later"}
                  {:at-ms (ms "2026-01-01T00:10:00.000") :kind :message :summary "earlier"}])]
    (is (= (ms "2026-01-01T00:10:00.000") (:start-ms (first placed))))
    (is (= ["earlier" "later"] (map :summary (:steps (first placed)))))))

(deftest index-lists-only-traceable-agents
  (let [html (str (hic/html
                    (page/index-body
                      {:zone utc
                       :sessions [{:session_id "a1" :agent "agy" :cwd "/x"
                                   :timestamp "2026-01-01T00:00:00.000"}
                                  {:session_id "c1" :agent "codex" :cwd "/y"
                                   :timestamp "2026-01-01T00:00:00.000"}]})))]
    (is (not (str/includes? html "session=a1")))
    (is (str/includes? html "session=c1"))))

(deftest index-shows-name-and-id-columns
  (let [html (str (hic/html
                    (page/index-body
                      {:zone utc :names {"codex:c1" {:name "Scrub turn digests"}}
                       :sessions [{:session_id "c1" :agent "codex" :cwd "/y"
                                   :timestamp "2026-01-01T00:00:00.000"}
                                  {:session_id "c2" :agent "codex" :cwd "/y"
                                   :timestamp "2026-01-01T00:00:00.000"}]})))]
    (is (str/includes? html "<th>name</th><th>id</th>"))
    (is (str/includes? html ">Scrub turn digests<"))
    (is (str/includes? html ">—<") "unnamed sessions show a dash in the name column")))

(deftest auto-names-and-statuses-display
  (let [names {"codex:c1" {:auto-name "scrub-rules" :status "Tightening host rules"}
               "codex:c2" {:name "Picked by a person" :auto-name "ignored-auto"}}
        html (str (hic/html
                    (page/index-body
                      {:zone utc :names names
                       :sessions [{:session_id "c1" :agent "codex" :cwd "/y"
                                   :timestamp "2026-01-01T00:00:00.000"}
                                  {:session_id "c2" :agent "codex" :cwd "/y"
                                   :timestamp "2026-01-01T00:00:00.000"}]})))]
    (is (str/includes? html "trace-auto"))
    (is (str/includes? html ">scrub-rules<"))
    (is (str/includes? html "Tightening host rules"))
    (is (str/includes? html ">Picked by a person<"))
    (is (not (str/includes? html "ignored-auto")) "a real name wins over the auto name")))
