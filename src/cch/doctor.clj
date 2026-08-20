(ns cch.doctor
  "cch doctor — a one-glance report of whether each agent on this box is
  actually wired to federate.

  Motivated by the class of silent gap where an agent's hooks look installed
  but never fire: a codex whose hooks were never trusted, a server that isn't
  running, a code hook disabled in the DB. Any one of those makes an agent go
  quiet with no error — this command surfaces them in one line instead of an
  afternoon of spelunking.

  Detection is split into PURE predicates over already-read content (easily
  tested with fixtures) and thin I/O wrappers that read the real files. The
  `detect-agents` aggregate is the shared 'which agents are on this box'
  primitive that `cch install --all` reuses (claude-code-hooks-7t1)."
  (:require [babashka.fs :as fs]
            [cheshire.core :as json]
            [cch.agents.agy :as agy]
            [cch.agents.codex :as codex]
            [cch.config-db :as cdb]
            [cli.codex-settings :as cs]
            [cli.registry :as registry]
            [cli.settings :as settings]
            [clojure.string :as str]))

(defn- home [] (System/getProperty "user.home"))

;; ---------------------------------------------------------------------------
;; Pure detectors — operate on already-parsed content, no I/O.
;; ---------------------------------------------------------------------------

(defn claude-dispatch-installed?
  "True if a parsed Claude settings map contains any cch dispatch entry
  (a hook whose URL points at the dispatcher's /dispatch/ path)."
  [settings]
  (boolean
    (some (fn [entries]
            (some (fn [entry]
                    (some #(some-> (:url %) (str/includes? "/dispatch/"))
                          (:hooks entry)))
                  entries))
          (vals (:hooks settings)))))

(defn codex-block-installed?
  "True if a codex config.toml string is wired to the cch dispatcher.

  Keys on the functional marker — a hook command that curls the dispatcher's
  /dispatch/ path tagged `X-CCH-Agent: codex` — rather than the sentinel
  comment, so it also recognizes installs written before the sentinel block
  format existed (same spirit as the claude detector, which matches the URL,
  not a tag)."
  [toml]
  (let [s (or toml "")]
    (or (and (str/includes? s "/dispatch/")
             (str/includes? s "X-CCH-Agent: codex"))
        (str/includes? s (str "# cch:begin " codex/block-name)))))

(defn agy-hooks-installed?
  "True if a parsed agy hooks.json (string keys) carries cch's dispatcher key."
  [hooks-json]
  (contains? (or hooks-json {}) agy/hook-key))

(defn codex-trusted-projects
  "Set of project paths marked `trust_level = \"trusted\"` in a codex
  config.toml string.

  This is a best-effort read of codex's project-trust model: codex gates hook
  execution on directory trust, so a project that isn't trusted runs without
  firing our dispatch hooks. Note codex versions have also used a per-hook
  content-hash TOFU stored in an internal state DB; we deliberately do NOT read
  that (its location is version-specific), so a :yes here means 'this dir is a
  trusted codex project', not a hard guarantee the hooks are trusted."
  [toml]
  (loop [lines (str/split-lines (or toml "")) cur nil acc #{}]
    (if-let [ln (first lines)]
      (let [t (str/trim ln)]
        (if-let [[_ path] (re-matches #"\[projects\.\"(.*)\"\]" t)]
          (recur (rest lines) path acc)
          (cond
            (re-matches #"\[.*\]" t)                       ; a different section
            (recur (rest lines) nil acc)
            (and cur (re-matches #"trust_level\s*=\s*\"trusted\"" t))
            (recur (rest lines) cur (conj acc cur))
            :else
            (recur (rest lines) cur acc))))
      acc)))

(defn path-covered-by?
  "True if `path` equals or descends from any path in `roots`. Used to ask
  whether a cwd falls under a trusted codex project."
  [roots path]
  (let [p (str path)]
    (boolean
      (some (fn [root]
              (let [r (str root)]
                (or (= p r) (str/starts-with? p (str r "/")))))
            roots))))

(defn enabled-code-hooks
  "Names of :code hooks enabled at global scope, given the global-scope DB
  rows. Pure over `rows` and the registry."
  [rows]
  (let [code? (into #{} (for [[n entry] (registry/list-hooks)
                              :when (= :code (registry/hook-type entry))]
                          n))]
    (into #{} (for [{:keys [hook-name enabled]} rows
                    :when (and enabled (code? hook-name))]
                hook-name))))

;; ---------------------------------------------------------------------------
;; I/O wrappers.
;; ---------------------------------------------------------------------------

(defn server-reachable?
  "Fast TCP connect probe against the dispatcher."
  [host port]
  (try
    (with-open [s (java.net.Socket.)]
      (.connect s (java.net.InetSocketAddress. ^String host (int port)) 400)
      true)
    (catch Exception _ false)))

(defn- read-agy-hooks [path]
  (when (fs/exists? path)
    (try (json/parse-string (slurp path)) (catch Exception _ nil))))

(defn detect-claude []
  (let [path (settings/global-settings-path)]
    {:agent      "claude-code"
     :present?   (fs/exists? (str (home) "/.claude"))
     :installed? (claude-dispatch-installed? (settings/read-settings path))
     :config     path}))

(defn detect-codex [cwd]
  (let [path (cs/codex-config-path)
        toml (cs/read-config path)
        trusted (codex-trusted-projects toml)]
    {:agent      "codex"
     :present?   (fs/exists? (cs/codex-home))
     :installed? (codex-block-installed? toml)
     :config     path
     :extra      {:trusted-projects (count trusted)
                  :cwd-trusted?     (path-covered-by? trusted cwd)}}))

(defn detect-agy []
  (let [path (agy/hooks-config-path)]
    {:agent      "agy"
     :present?   (fs/exists? (str (home) "/.gemini"))
     :installed? (agy-hooks-installed? (read-agy-hooks path))
     :config     path
     :extra      {:status-line? (fs/exists? (agy/script-path))}}))

(defn detect-agents
  "Per-agent detection for this box. `cwd` scopes codex's directory-trust
  check. Shared by `cch doctor` and `cch install --all`."
  [cwd]
  [(detect-claude) (detect-codex cwd) (detect-agy)])

(defn box-status
  "Box-level facts shared by every agent: is the dispatcher up, and how many
  code hooks are enabled globally."
  []
  {:server {:host "127.0.0.1" :port 8888
            :reachable? (server-reachable? "127.0.0.1" 8888)}
   :hooks  {:enabled (enabled-code-hooks (cdb/list-for-scope cdb/global-scope))}})

;; ---------------------------------------------------------------------------
;; Report model + rendering.
;; ---------------------------------------------------------------------------

(defn hostname []
  (try (.getHostName (java.net.InetAddress/getLocalHost))
       (catch Exception _ "this box")))

(defn agent-note
  "Human-readable note for an agent row: the agent-specific caveat, most
  importantly codex's trust status."
  [{:keys [agent installed? present? extra]}]
  (cond
    (not present?)  "not on this box"
    (not installed?) "cch not wired — run `cch install`"
    (= agent "codex")
    (let [{:keys [trusted-projects cwd-trusted?]} extra]
      (cond
        cwd-trusted?
        (format "trust: this dir is a trusted codex project (%d total)" trusted-projects)
        (pos? (or trusted-projects 0))
        (format "trust: ⚠ this dir is NOT a trusted codex project (%d others are) — hooks may be skipped" trusted-projects)
        :else
        "trust: ⚠ no trusted codex projects — run codex interactively once here to trust"))
    (= agent "agy")
    (if (:status-line? extra) "status-line adapter linked" "—")
    :else "—"))

(defn- dot [ok?] (if ok? "●" "○"))

(defn problems
  "Actionable problems worth a nonzero exit: the server is down, or an agent
  present on this box has cch not wired. Codex trust is advisory (best-effort,
  version-dependent) so it warns but never fails the exit code."
  [box agents]
  (cond-> []
    (not (get-in box [:server :reachable?]))
    (conj "dispatcher not reachable — code-hook dispatch will fail")
    :always
    (into (for [{:keys [agent present? installed?]} agents
                :when (and present? (not installed?))]
            (format "%s present but cch not wired" agent)))))

(defn render
  "Render the full doctor report as a string."
  [box agents]
  (let [{:keys [server hooks]} box
        probs (problems box agents)]
    (str/join
      "\n"
      (concat
        [(format "cch doctor — %s" (hostname))
         ""
         (format "  server   http://%s:%d   %s"
                 (:host server) (:port server)
                 (if (:reachable? server) "● reachable" "○ NOT reachable"))
         (format "  hooks    %d code hook(s) enabled globally"
                 (count (:enabled hooks)))
         ""
         (format "  %-13s %-10s %s" "agent" "cch wired" "notes")
         (str "  " (apply str (repeat 60 "─")))]
        (for [{:keys [installed?] :as a} agents]
          (format "  %-13s %-10s %s"
                  (:agent a)
                  (str (dot installed?) " " (if installed? "yes" "no"))
                  (agent-note a)))
        [""
         "  legend: ● ok   ○ absent/not-wired   ⚠ needs attention"]
        (when (seq probs)
          (cons "" (cons "  problems:"
                         (for [p probs] (str "    ⚠ " p)))))))))

(defn run
  "cch doctor [--cwd DIR] — report per-agent federation wiring on this box.
  Exits nonzero if the dispatcher is down or an agent is installed-but-unwired."
  [& args]
  (if (some #{"--help" "-h"} args)
    (do (println "cch doctor [--cwd DIR]")
        (println)
        (println "Report whether each agent on this box is wired to federate:")
        (println "  installed?  cch dispatch hooks present in the agent's config")
        (println "  enabled?    code hooks enabled globally in the cch DB")
        (println "  reachable?  the cch dispatcher is up at 127.0.0.1:8888")
        (println "  trusted?    (codex) this dir is a trusted codex project"))
    (let [kv     (apply hash-map args)
          cwd    (or (get kv "--cwd") (System/getProperty "user.dir"))
          box    (box-status)
          agents (detect-agents cwd)]
      (println (render box agents))
      (System/exit (if (seq (problems box agents)) 1 0)))))
