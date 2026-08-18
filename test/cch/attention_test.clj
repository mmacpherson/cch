(ns cch.attention-test
  (:require [clojure.test :refer [deftest is testing]]
            [cch.attention :as att]
            [clojure.string :as str]))

(deftest since-clause-windows
  (testing "nil means all history — no restriction"
    (is (= "" (att/since-clause nil))))
  (testing "a day count restricts on timestamp"
    (is (str/includes? (att/since-clause 30) "-30 days"))
    (is (str/includes? (att/since-clause 30) "timestamp >="))))

;; Build the home-relative path from the actual user home so the abbreviator
;; (which uses host $HOME) collapses it to ~ on any OS, and so no username
;; path is hardcoded into the repo.
(def home (System/getProperty "user.home"))
(def home-project (str home "/projects/scratch/demo"))

(def sample
  {:days 30
   :limit 2
   :kinds [{:agent "claude-code" :kind "idle_prompt"
            :episodes 2604 :hours 170.6 :avg_secs 236}
           {:agent "codex" :kind "permission_request"
            :episodes 1802 :hours 34.8 :avg_secs 69}]
   :projects [{:cwd home-project :episodes 173 :hours 10.3}
              {:cwd "/elsewhere/thing" :episodes 5 :hours 0.4}]})

(deftest render-reports-figures
  (let [out (att/render sample)]
    (testing "names the window so a figure is never read without its denominator"
      (is (str/includes? out "last 30 days")))
    (testing "each kind appears with its episode count and hours"
      (is (str/includes? out "idle_prompt"))
      (is (str/includes? out "2604"))
      (is (str/includes? out "170.6")))
    (testing "home is abbreviated, other paths left alone"
      (is (str/includes? out "~/projects/scratch/demo"))
      (is (str/includes? out "/elsewhere/thing")))
    (testing "the cap is stated, since it changes what the total means"
      (is (str/includes? out (str att/walked-away-secs))))))

(deftest render-tolerates-empty
  (testing "no episodes yet is a valid state, not an error"
    (let [out (att/render {:kinds [] :projects [] :days 1})]
      (is (str/includes? out "last 1 days"))))
  (testing "all history when days is absent"
    (is (str/includes? (att/render {:kinds [] :projects []}) "all history"))))
