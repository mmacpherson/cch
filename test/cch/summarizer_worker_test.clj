(ns cch.summarizer-worker-test
  (:require [cch.summarizer-worker :as w]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]))

(deftest settings-default-off-and-normalize
  (let [s (w/settings nil)]
    (is (false? (:enabled s)) "the summarizer is opt-in")
    (is (= "claude-haiku-5-5" (:model s)))
    (is (= 1.0 (:daily-usd-cap s))))
  (let [s (w/settings {:summarizer {:enabled true :key-file "~/k"
                                    :known-hosts {:hostA "runner-a" :hostB nil}}})]
    (is (true? (:enabled s)))
    (is (str/ends-with? (:key-file s) "/k"))
    (is (not (str/starts-with? (:key-file s) "~")))
    (is (= {"hostA" "runner-a" "hostB" nil} (:known-hosts s))
        "YAML keyword keys become exact host strings")))

(deftest fleet-context-excludes-self-and-stale
  (let [now 1000000
        summaries {"claude:me" {:auto-name "me" :repo "r" :status "s" :updated-at now}
                   "codex:a" {:auto-name "a" :repo "r" :status "doing a" :updated-at (- now 10)}
                   "codex:old" {:auto-name "old" :repo "r" :status "x" :updated-at (- now 999999)}}]
    (is (= [{:name "a" :repo "r" :status "doing a"}]
           (w/fleet-context summaries "claude:me" now 1000)))))

(deftest unnamed-sessions-are-named-from-several-turns
  (let [turn (fn [p tool reply] {:prompts [{:text p}]
                                 :steps [{:kind :tool :summary tool}]
                                 :closing-message reply :ended-at "t"})
        turns [(turn "one" "t1" "r1") (turn "two" "t2" "r2")
               (turn "three" "t3" "r3") (turn "four" "t4" "r4")]]
    (testing "a named session sends only the latest turn"
      (is (= {:prompt "four" :tool-summaries ["t4"] :closing-message "r4"}
             (w/naming-input turns true))))
    (testing "an unnamed session sends its last three turns"
      (let [in (w/naming-input turns false)]
        (is (= ["t2" "t3" "t4"] (:tool-summaries in)))
        (is (str/includes? (:prompt in) "two"))
        (is (not (str/includes? (:prompt in) "one")))
        (is (= "r4" (:closing-message in)))))))

(deftest routes-follow-the-agent
  (is (= "codex:x" (w/route-id "codex" "x")))
  (is (= "claude:x" (w/route-id "claude-code" "x"))))

(deftest only-priced-models-run
  (is (contains? w/priced-models "claude-haiku-5-5"))
  (is (not (contains? w/priced-models "claude-opus-5-5"))))
