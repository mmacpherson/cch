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
