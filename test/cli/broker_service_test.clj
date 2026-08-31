(ns cli.broker-service-test
  "Tests for cch install-broker-service — the generic broker deploy primitive.

  Coverage is the pure piece: rendering the systemd unit from the template with
  the operator's host/port. Writing a real unit needs a live systemd and isn't
  portable in CI."
  (:require [cli.broker-service :as broker]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]))

(deftest render-unit-substitutes-host-and-port
  (testing "template placeholders are replaced and the runtime broker is invoked"
    (let [rendered (broker/render-unit "127.0.0.1" 8787)]
      (is (not (str/includes? rendered "{{HOST}}")))
      (is (not (str/includes? rendered "{{PORT}}")))
      (is (str/includes? rendered
                         "%h/.local/share/cch/runtime/bin/cch control broker --host 127.0.0.1 --port 8787")
          "runs the self-contained runtime broker with the given host/port")
      (is (str/includes? rendered "EnvironmentFile=%h/.config/cch-control-broker.env")
          "config comes from the operator-provided env file")
      (is (str/includes? rendered "Restart=on-failure")))))

(deftest render-unit-honors-nondefault-values
  (testing "a custom host/port flows into ExecStart"
    (let [rendered (broker/render-unit "0.0.0.0" 9099)]
      (is (str/includes? rendered "--host 0.0.0.0 --port 9099")))))

(deftest env-example-documents-every-broker-key
  (testing "the shipped env example lists every variable the broker reads"
    (let [example (slurp (io/resource "cch-control-broker.env.example"))]
      (doseq [k ["CCH_CONTROL_RUNNER_TOKENS"
                 "CCH_CONTROL_DATABASE_URL"
                 "CCH_CONTROL_DATABASE_USER"
                 "CCH_CONTROL_DATABASE_PASSWORD"
                 "CCH_CONTROL_DATABASE_SCHEMA"
                 "CCH_CONTROL_DATABASE_POOL_SIZE"
                 "CCH_CONTROL_WEB_HOST"
                 "CCH_CONTROL_WEB_PORT"]]
        (is (str/includes? example k) (str "documents " k))))))
