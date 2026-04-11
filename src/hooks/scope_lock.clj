(ns hooks.scope-lock
  "PreToolUse hook for Edit/Write that enforces file scope.

  Default: allows edits anywhere within the git worktree root.
  Narrowed: if a .scope-lock.edn is found walking up from cwd,
  only allows edits under the listed :allowed-paths (relative to worktree root)."
  (:require [cch.core :refer [defhook]]
            [cch.config :as config]
            [cch.protocol :as proto]
            [clojure.string :as str]
            [babashka.process :as p]))

(defn worktree-root
  "Returns the git worktree root, or nil if not in a repo.
  Called once per hook invocation (each invocation is a separate bb process).
  Cross-process memoization is impossible in the command hook model and
  unnecessary — git rev-parse is <5ms."
  []
  (let [result (p/sh ["git" "rev-parse" "--show-toplevel"])]
    (when (zero? (:exit result))
      (str/trim (:out result)))))

(defn check-scope
  "Pure logic: check whether file-path is allowed.
  Returns nil if allowed, or a decision map with :decision and :reason."
  [file-path cwd root]
  (when (and file-path root)
    (cond
      ;; Always allow /tmp — universal scratch space
      (str/starts-with? file-path "/tmp/")
      nil

      ;; Always hard-deny writes into .git/
      (str/includes? file-path "/.git/")
      {:decision :deny
       :reason   (str "scope-lock: blocked edit inside .git/: " file-path)}

      ;; Outside worktree — ask user
      (not (str/starts-with? file-path (str root "/")))
      {:decision :ask
       :reason   (str "scope-lock: edit outside worktree (" root "): " file-path)}

      ;; Check for narrowing config (bounded to worktree root)
      :else
      (when-let [config-path (config/find-config-up (or cwd root) ".scope-lock.edn" root)]
        (let [cfg           (config/load-edn config-path)
              allowed-paths (:allowed-paths cfg)]
          (when (seq allowed-paths)
            (let [rel-path (subs file-path (inc (count root)))
                  allowed? (some #(str/starts-with? rel-path %) allowed-paths)]
              (when-not allowed?
                {:decision :ask
                 :reason   (str "scope-lock: edit outside allowed scope\n"
                                "  file:    " rel-path "\n"
                                "  allowed: " (str/join ", " allowed-paths) "\n"
                                "  config:  " config-path)}))))))))

(defhook scope-lock
  "Enforces file edit scope per worktree."
  {}
  [input]
  (let [file-path (proto/extract-file-path input)
        cwd       (:cwd input)
        root      (worktree-root)]
    (check-scope file-path cwd root)))
