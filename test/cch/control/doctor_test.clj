(ns cch.control.doctor-test
  (:require [babashka.fs :as fs]
            [cch.control.doctor :as doctor]
            [cheshire.core :as json]
            [clojure.test :refer [deftest is]]))

(defn- schema-runner [methods]
  (fn [argv]
    (cond
      (= ["claude" "--version"] argv)
      {:exit 0 :out "2.1.241\n" :err ""}

      (= ["codex" "--version"] argv)
      {:exit 0 :out "codex-cli 0.149.0\n" :err ""}

      (= ["codex" "app-server" "generate-json-schema" "--experimental" "--out"]
         (pop argv))
      (do
        (spit (str (peek argv) "/ClientRequest.json")
              (json/generate-string {:methods methods}))
        {:exit 0 :out "" :err ""})

      :else {:exit 1 :out "" :err "unexpected"})))

(deftest report-checks-generated-protocol-without-exposing-identifiers-or-token
  (let [directory (fs/create-temp-dir {:prefix "cch-doctor-test-"})
        deleted (atom nil)
        report
        (doctor/report
          {:run-command (schema-runner doctor/required-codex-methods)
           :create-temp-dir (constantly directory)
           :delete-tree #(do (reset! deleted %) (fs/delete-tree %))
           :list-local-sessions
           (constantly {:sessions [{:agent "claude"} {:agent "codex"}]
                        :errors []})
           :pairing-config
           (constantly {:url "https://broker.invalid"
                        :runner-id "runner-private"
                        :token "synthetic-secret-token"})
           :runner-status (constantly {:status :active})})]
    (is (= :ok (:status report)))
    (is (= :configured (get-in report [:pairing :status])))
    (is (= :ok (get-in report [:providers :codex :protocol :status])))
    (is (= 1 (get-in report [:providers :claude :sessions])))
    (is (= (str directory) @deleted))
    (is (not (re-find #"runner-private|synthetic-secret-token|broker.invalid"
                      (pr-str report))))))

(deftest missing-required-codex-method-is-actionable
  (let [directory (fs/create-temp-dir {:prefix "cch-doctor-missing-test-"})
        report
        (doctor/report
          {:run-command (schema-runner #{"thread/loaded/list" "thread/read"})
           :create-temp-dir (constantly directory)
           :delete-tree fs/delete-tree
           :list-local-sessions (constantly {:sessions [] :errors []})
           :pairing-config (constantly nil)
           :runner-status (constantly {:status :inactive})})]
    (is (= :attention (:status report)))
    (is (= :unsupported
           (get-in report [:providers :codex :protocol :status])))
    (is (= ["thread/queue/add"]
           (get-in report [:providers :codex :protocol :missing-methods])))
    (is (string? (get-in report [:providers :codex :protocol :action])))))

(deftest failing-runner-and-missing-broker-routes-need-attention
  ;; Supervision alone said "active" while the runner failed every tick and
  ;; the broker held none of its routes. Both are now reported.
  (let [directory (fs/create-temp-dir {:prefix "cch-doctor-exchange-test-"})
        report
        (doctor/report
          {:run-command (schema-runner doctor/required-codex-methods)
           :create-temp-dir (constantly directory)
           :delete-tree fs/delete-tree
           :list-local-sessions
           (constantly {:sessions [{:agent "claude" :available true
                                    :id "claude:private-route"}]
                        :errors []})
           :pairing-config
           (constantly {:url "https://broker.invalid"
                        :runner-id "runner-private"
                        :token "synthetic-secret-token"})
           :runner-status (constantly {:status :active})
           :runner-health (constantly {:status :failing
                                       :consecutive-failures 42
                                       :error-type "broker-unavailable"})
           :broker-sessions (constantly [{:id "claude:other-route"
                                          :runner-id "runner-other"}])})]
    (is (= :attention (:status report)))
    (is (= :failing (get-in report [:runner :exchange :status])))
    (is (= 42 (get-in report [:runner :exchange :consecutive-failures])))
    (is (string? (get-in report [:runner :exchange :action])))
    (is (= {:status :missing-routes :routes 0 :local-available 1}
           (dissoc (:broker report) :action)))
    (is (not (re-find #"runner-private|synthetic-secret-token|broker.invalid|private-route"
                      (pr-str report))))))

(deftest unreachable-broker-is-reported-without-its-address
  (let [directory (fs/create-temp-dir {:prefix "cch-doctor-broker-test-"})
        report
        (doctor/report
          {:run-command (schema-runner doctor/required-codex-methods)
           :create-temp-dir (constantly directory)
           :delete-tree fs/delete-tree
           :list-local-sessions (constantly {:sessions [] :errors []})
           :pairing-config (constantly {:url "https://broker.invalid"
                                        :runner-id "runner-private"
                                        :token "synthetic-secret-token"})
           :runner-status (constantly {:status :active})
           :runner-health (constantly {:status :ok})
           :broker-sessions (fn [_] (throw (ex-info "Broker request failed"
                                                    {:type :broker-unavailable})))})]
    (is (= :attention (:status report)))
    (is (= :unreachable (get-in report [:broker :status])))
    (is (= :broker-unavailable (get-in report [:broker :error-type])))))
