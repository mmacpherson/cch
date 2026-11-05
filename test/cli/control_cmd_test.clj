(ns cli.control-cmd-test
  (:require [cch.subprocess :as subprocess]
            [babashka.fs :as fs]
            [cheshire.core :as json]
            [cch.control.codex :as codex]
            [cch.control.mcp-http :as mcp-http]
            [cli.codex-settings :as codex-settings]
            [cli.control-cmd :as control-cmd]
            [clojure.string :as str]
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
                (fn [expected]
                  (is (contains? (get-in expected ["send_message" :parameters])
                                 "in_reply_to")
                      "the CLI checks the reload against this build's tools")
                  {:tools ["get_session" "list_sessions"
                           "send_message" "set_session_alias"]})]
    (let [output (with-out-str (control-cmd/run "refresh-mcp" "codex"))]
      (is (re-find #"refreshed and verified" output))
      (is (re-find #"No Codex agent or app-server process was restarted" output)))))

(deftest refresh-mcp-claude-gives-session-local-command
  (let [output (with-out-str (control-cmd/run "refresh-mcp" "claude"))]
    (is (re-find #"local to each active session" output))
    (is (re-find #"/mcp reconnect cch" output))))

(deftest one-shot-commands-report-typed-errors-on-one-line
  (let [exit-code (atom nil)
        err (java.io.StringWriter.)]
    (with-redefs [codex/refresh-mcp! (fn [_]
                                       (throw (ex-info "synthetic refresh failure"
                                                       {:type :codex-mcp-stale})))
                  control-cmd/exit! (fn [code] (reset! exit-code code))]
      (binding [*err* err]
        (control-cmd/run "refresh-mcp" "codex")))
    (is (= 1 @exit-code))
    (is (= "Error: synthetic refresh failure\n" (str err)))))

(deftest untyped-errors-keep-their-stack-trace
  (with-redefs [codex/refresh-mcp! (fn [_] (throw (ex-info "bug" {})))]
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"bug"
                          (control-cmd/run "refresh-mcp" "codex")))))

(deftest control-uninstall-is-scoped-and-idempotent
  (let [dir (fs/create-temp-dir {:prefix "cch-uninstall-"})
        paths {:codex-path (str dir "/config.toml") :claude-path (str dir "/claude.json")
               :settings-path (str dir "/settings.json") :env-path (str dir "/codex-mcp.env")}
        other {:command "other-mcp"}
        claude {:mcpServers {:cch {:type "http" :url "http://127.0.0.1:8888/mcp"} :other other}
                :projects {:example {:unchanged true}}}
        settings {:hooks {:SessionStart [{:hooks [{:command "cch control register-claude"
                                                   :__cch "control-plane:claude-registration"}
                                                  {:command "user-hook"}]}]
                           :PreToolUse [{:hooks [{:type "http" :url "http://127.0.0.1:8888/dispatch/PreToolUse"}]}]}
                  :permissions {:allow ["Read" "mcp__cch__send_message" "mcp__other__tool"]}}]
    (try
      (spit (:codex-path paths) (str "model = 'example'\n"
                                    (codex-settings/render-control-mcp-http-block {:url "http://127.0.0.1:8888/mcp"})))
      (spit (:claude-path paths) (json/generate-string claude))
      (spit (:settings-path paths) (json/generate-string settings))
      (spit (:env-path paths) (str "CCH_MCP_TOKEN=" "synthetic-value"))
      (with-redefs [subprocess/run (fn [& _] (throw (ex-info "No services or provider commands" {})))]
        (let [result (control-cmd/uninstall-control! paths)
              cleaned (json/parse-string (slurp (:claude-path paths)) true)
              clean-settings (json/parse-string (slurp (:settings-path paths)) true)]
          (is (get-in result [:codex :mcp-removed?]))
          (is (get-in result [:claude :mcp-removed?]))
          (is (:environment-file-removed? result))
          (is (= {:other other} (:mcpServers cleaned)))
          (is (= (:projects claude) (:projects cleaned)))
          (is (= [{:command "user-hook"}] (get-in clean-settings [:hooks :SessionStart 0 :hooks])))
          (is (= (get-in settings [:hooks :PreToolUse]) (get-in clean-settings [:hooks :PreToolUse])))
          (is (= ["Read" "mcp__other__tool"] (get-in clean-settings [:permissions :allow])))
          (is (str/includes? (slurp (:codex-path paths)) "model = 'example'"))
          (is (not (fs/exists? (:env-path paths))))
          (when (.contains (.supportedFileAttributeViews (java.nio.file.FileSystems/getDefault)) "posix")
            (doseq [path [(:claude-path paths) (:settings-path paths) (:codex-path paths)]]
              (is (= #{java.nio.file.attribute.PosixFilePermission/OWNER_READ
                       java.nio.file.attribute.PosixFilePermission/OWNER_WRITE}
                     (set (java.nio.file.Files/getPosixFilePermissions
                            (fs/path path) (make-array java.nio.file.LinkOption 0))))))))
        (let [again (control-cmd/uninstall-control! paths)]
          (is (not (get-in again [:codex :mcp-removed?])))
          (is (not (get-in again [:claude :mcp-removed?])))
          (is (not (:settings-updated? again)))
          (is (not (:environment-file-removed? again)))))
      (finally (fs/delete-tree dir)))))

(deftest uninstall-help-never-removes-config
  (with-redefs [control-cmd/uninstall-control! (fn [& _] (throw (ex-info "Must not uninstall" {})))]
    (is (= "Usage: cch control uninstall\n" (with-out-str (control-cmd/run "uninstall" "--help"))))
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"Usage"
                          (#'control-cmd/dispatch "uninstall" "--unknown")))))

(deftest control-uninstall-preserves-unknown-server-and-preflights-json
  (let [dir (fs/create-temp-dir {:prefix "cch-uninstall-"})
        paths {:codex-path (str dir "/config.toml") :claude-path (str dir "/claude.json")
               :settings-path (str dir "/settings.json") :env-path (str dir "/codex-mcp.env")}
        codex "[mcp_servers.cch]\ncommand = 'other-mcp'\n"
        claude {:mcpServers {:cch {:command "other-mcp" :args []}}}]
    (try
      (spit (:codex-path paths) codex)
      (spit (:claude-path paths) (json/generate-string claude))
      (spit (:settings-path paths) "{malformed")
      (is (thrown-with-msg? clojure.lang.ExceptionInfo #"files left unchanged"
                            (control-cmd/uninstall-control! paths)))
      (is (= codex (slurp (:codex-path paths))))
      (is (= claude (json/parse-string (slurp (:claude-path paths)) true)))
      (spit (:settings-path paths) "{}")
      (let [result (control-cmd/uninstall-control! paths)]
        (is (get-in result [:codex :preserved-mcp?]))
        (is (get-in result [:claude :preserved-mcp?]))
        (is (= codex (slurp (:codex-path paths))))
        (is (= claude (json/parse-string (slurp (:claude-path paths)) true))))
      (finally (fs/delete-tree dir)))))
