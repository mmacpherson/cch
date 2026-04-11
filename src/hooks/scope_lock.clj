(ns hooks.scope-lock
  "PreToolUse hook for Edit/Write that enforces file scope.

  Default: allows edits anywhere within the git worktree root.
  Narrowed: if a .scope-lock.edn is found walking up from cwd,
  only allows edits under the listed :allowed-paths (relative to worktree root)."
  (:require [cch.core :refer [defhook]]
            [cch.config :as config]
            [cch.protocol :as proto]
            [clojure.string :as str]
            [babashka.process :as p]
            [babashka.fs :as fs]))

(defn worktree-root
  "Returns the git worktree root for the given cwd, or nil if not in a repo.
  Uses git -C to query from the correct directory regardless of where
  bb was launched from."
  [cwd]
  (let [result (p/sh ["git" "-C" (or cwd ".") "rev-parse" "--show-toplevel"])]
    (when (zero? (:exit result))
      (str/trim (:out result)))))

(defn- normalize-path
  "Canonicalize a path, resolving symlinks and .."
  [path]
  (when path
    (str (fs/canonicalize path {:nofollow-links false}))))

(defn- path-segments
  "Split a path into segments."
  [path]
  (remove str/blank? (str/split path #"/")))

(defn- path-starts-with-segments?
  "Check if path segments start with prefix segments.
  Safer than string prefix — prevents 'src' matching 'src-old'."
  [path-segs prefix-segs]
  (and (<= (count prefix-segs) (count path-segs))
       (= prefix-segs (take (count prefix-segs) path-segs))))

(defn check-scope
  "Pure logic: check whether file-path is allowed.
  Takes only data — no I/O. Returns nil if allowed, or a decision map.

  allowed-paths is a seq of path prefixes relative to root, or nil for
  unrestricted access within the worktree."
  [file-path root allowed-paths]
  (when (and file-path root)
    (let [file-path (normalize-path file-path)
          root      (normalize-path root)]
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

        ;; Check narrowing via allowed-paths
        (seq allowed-paths)
        (let [rel-path     (subs file-path (inc (count root)))
              rel-segments (path-segments rel-path)
              allowed?     (some (fn [ap]
                                  (path-starts-with-segments?
                                    rel-segments
                                    (path-segments ap)))
                                 allowed-paths)]
          (when-not allowed?
            {:decision :ask
             :reason   (str "scope-lock: edit outside allowed scope\n"
                            "  file:    " rel-path "\n"
                            "  allowed: " (str/join ", " allowed-paths))}))

        ;; Inside worktree, no narrowing — allow
        :else nil))))

(defhook scope-lock
  "Enforces file edit scope per worktree."
  {}
  [input]
  (let [file-path    (proto/extract-file-path input)
        cwd          (:cwd input)
        root         (worktree-root cwd)
        ;; Load narrowing config (I/O happens here, not in check-scope)
        config-path  (when root
                       (config/find-config-up (or cwd root) ".scope-lock.edn" root))
        cfg          (config/load-edn config-path)
        allowed-paths (:allowed-paths cfg)]
    (check-scope file-path root allowed-paths)))
