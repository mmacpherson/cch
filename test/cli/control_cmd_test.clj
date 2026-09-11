(ns cli.control-cmd-test
  (:require [cch.subprocess :as subprocess]
            [cch.control.codex :as codex]
            [cch.control.mcp-http :as mcp-http]
            [cli.codex-settings :as codex-settings]
            [cli.control-cmd :as control-cmd]
            [clojure.test :refer [deftest is testing]]))

(deftest claude-http-add-args-registers-http-transport-with-bearer-header
  (let [args (control-cmd/claude-http-add-args "http://127.0.0.1:8888/mcp" "tok-abc")]
    (is (= ["claude" "mcp" "add" "--scope" "user" "--transport" "http" "cch"
            "http://127.0.0.1:8888/mcp"
            "--header" "Authorization: Bearer tok-abc"]
           args))
    (testing "the token rides a header, not a subprocess env/arg"
      (is (not (some #{"--env"} args))))))

(deftest install-mcp-reconciles-provider-environment
  ;; Both agents now point at the shared HTTP endpoint: Claude via `mcp add
  ;; --transport http` with a bearer header, Codex via the url MCP block.
  (let [tokens {"codex-tok" "codex" "claude-tok" "claude"}
        url "http://127.0.0.1:8888/mcp"]
    (doseq [[agent expected-remove expected-add]
            [[:claude
              ["claude" "mcp" "remove" "cch" "--scope" "user"]
              ["claude" "mcp" "add" "--scope" "user" "--transport" "http" "cch"
               url "--header" "Authorization: Bearer claude-tok"]]
             [:codex
              ["codex" "mcp" "remove" "cch"]
              nil]]]
      (testing (name agent)
        (let [calls (atom [])
              installed (atom nil)]
          (with-redefs [subprocess/run
                        (fn [argv]
                          (swap! calls conj argv)
                          {:exit 0 :out "" :err ""})
                        mcp-http/endpoint-url (constantly url)
                        codex-settings/install-control-mcp-http!
                        (fn [path config]
                          (reset! installed [path config]))]
            (is (= :updated
                   (#'control-cmd/install-mcp!
                     agent "/home/example/.config/codex" "/opt/cch/bin/cch"
                     "/home/example/.config/cch/control-runner.json"
                     "revision-1" tokens)))
            (is (= (cond-> [[(name agent) "--version"]
                            [(name agent) "mcp" "get" "cch"]
                            expected-remove]
                     expected-add (conj expected-add)
                     (= :codex agent) (conj ["codex" "mcp" "get" "cch"]))
                   @calls))
            (if (= :codex agent)
              (is (= ["/home/example/.config/codex/config.toml" {:url url}]
                     @installed))
              (is (nil? @installed)))))))))

(deftest install-mcp-adds-when-not-configured
  (let [calls (atom [])
        installed (atom nil)
        get-count (atom 0)]
    (with-redefs [subprocess/run
                  (fn [argv]
                    (swap! calls conj argv)
                    {:exit (if (and (= ["codex" "mcp" "get" "cch"] argv)
                                    (= 1 (swap! get-count inc)))
                             1
                             0)
                     :out "" :err "not found"})
                  codex-settings/install-control-mcp-http!
                  (fn [path config]
                    (reset! installed [path config]))]
      (is (= :installed
             (#'control-cmd/install-mcp!
               :codex "/home/example/.codex" "/opt/cch/bin/cch"
               "/home/example/.config/cch/control-runner.json"
               "revision-1" {"codex-tok" "codex" "claude-tok" "claude"})))
      (is (= [["codex" "--version"]
              ["codex" "mcp" "get" "cch"]
              ["codex" "mcp" "get" "cch"]]
             @calls))
      (is (= "/home/example/.codex/config.toml" (first @installed))))))

(deftest provider-mcp-uses-absolute-cch-and-does-not-copy-runner-token
  (let [add (get (#'control-cmd/mcp-commands
                   :claude "/home/example/.config/codex" "/opt/cch/bin/cch"
                   "/home/example/.config/cch/control-runner.json"
                   "revision-1") :add)]
    (is (= ["--" "/opt/cch/bin/cch" "control" "mcp"]
           (subvec add 14)))
    (is (not-any? #(re-find #"TOKEN|synthetic-token" %) add))))

(deftest refresh-mcp-codex-uses-native-app-server-refresh
  (with-redefs [codex/refresh-mcp!
                (fn [] {:tools ["get_session" "list_sessions"
                                "send_message" "set_session_alias"]})]
    (let [output (with-out-str (control-cmd/run "refresh-mcp" "codex"))]
      (is (re-find #"refreshed and verified" output))
      (is (re-find #"No Codex agent or app-server process was restarted" output)))))

(deftest refresh-mcp-claude-gives-session-local-command
  (let [output (with-out-str (control-cmd/run "refresh-mcp" "claude"))]
    (is (re-find #"local to each active session" output))
    (is (re-find #"/mcp reconnect cch" output))))
