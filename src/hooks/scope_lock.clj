(ns hooks.scope-lock
  "PreToolUse hook for Edit/Write that enforces file scope.

  Default: allows edits anywhere within the git worktree root.
  Narrowed: if a .cch-config.yaml is found walking up from cwd and
  contains hooks.scope-lock.allowed-paths, only allows edits under
  those path prefixes (relative to worktree root).

  Global allowlist: ~/.config/cch/config.yaml may contain
  hooks.scope-lock.global-allowed-paths with a list of absolute (or
  home-relative) paths that are always auto-allowed regardless of
  which worktree is active. Useful for paths Claude legitimately
  edits across all projects (e.g. ~/.claude, ~/.config/fish).

  Fails closed: if the project .cch-config.yaml exists but is
  malformed YAML, scope-lock denies rather than falling through to
  the unrestricted default."
  (:require [cch.core :refer [defhook]]
            [cch.config :as config]
            [cch.protocol :as proto]
            [clojure.string :as str]
            [babashka.fs :as fs]))

(defn worktree-root
  "Git worktree root for cwd, or nil if cwd isn't in a repo.
  Thin passthrough to the memoized cch.config/worktree-root — shares
  the cache with the dispatcher so the same cwd isn't re-shelled-out
  per hook invocation."
  [cwd]
  (config/worktree-root cwd))

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

(defn- expand-home
  "Expand a leading ~ to the user home directory."
  [path]
  (if (str/starts-with? path "~")
    (str (System/getProperty "user.home") (subs path 1))
    path))

(defn- normalize-global-path
  "Expand home and normalize a global-allowed path. Returns nil on error."
  [path]
  (try
    (normalize-path (expand-home path))
    (catch Exception _ nil)))

(defn check-scope
  "Pure logic: check whether file-path is allowed.
  Takes only data — no I/O. Returns nil if allowed, or a decision map.

  allowed-paths: seq of path prefixes relative to root (per-project narrowing).
  global-allowed-paths: seq of absolute or home-relative path prefixes
    (user-level allowlist, from ~/.config/cch/config.yaml)."
  ([file-path root allowed-paths]
   (check-scope file-path root allowed-paths nil))
  ([file-path root allowed-paths global-allowed-paths]
   (when (and file-path root)
     (let [file-path    (normalize-path file-path)
           root         (normalize-path root)
           file-segs    (path-segments file-path)
           global-segs  (keep #(some-> % normalize-global-path path-segments)
                               global-allowed-paths)]
       (cond
         ;; Always hard-deny writes into or targeting .git, even under /tmp
         (or (str/includes? file-path "/.git/")
             (str/ends-with? file-path "/.git"))
         {:decision :deny
          :reason   (str "scope-lock: blocked edit inside .git/: " file-path)}

         ;; Always allow /tmp — universal scratch space
         (str/starts-with? file-path "/tmp/")
         nil

         ;; User-level global allowlist (from ~/.config/cch/config.yaml)
         (some #(path-starts-with-segments? file-segs %) global-segs)
         nil

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
         :else nil)))))

(defhook scope-lock
  "Enforces file edit scope per worktree."
  {}
  [input]
  (let [file-path      (proto/extract-file-path input)
        cwd            (:cwd input)
        root           (worktree-root cwd)
        config-path    (when root
                         (config/find-config-up (or cwd root) ".cch-config.yaml" root))
        cfg            (try
                         (config/load-yaml config-path)
                         (catch clojure.lang.ExceptionInfo _e
                           ::malformed))
        global-cfg     (try
                         (config/load-yaml (config/global-config-path))
                         (catch Exception _ nil))
        global-allowed (get-in global-cfg [:hooks :scope-lock :global-allowed-paths])]
    (if (= cfg ::malformed)
      {:decision :deny
       :reason   (str "scope-lock: malformed config at " config-path
                      " — refusing to load (fail closed)")}
      (check-scope file-path root
                   (get-in cfg [:hooks :scope-lock :allowed-paths])
                   global-allowed))))
