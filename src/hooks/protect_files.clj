(ns hooks.protect-files
  "PreToolUse hook for Edit/Write that hard-denies edits to sensitive paths.

  Default deny set covers the obvious credentials/secret surfaces:
    .env / .env.* / .git/** / *.pem / *.key / id_rsa / id_ed25519 /
    .ssh/** / .aws/credentials / .netrc / .npmrc / secrets/** /
    *.kdbx / *.keystore

  Config at hooks.protect-files in .cch-config.yaml extends the list:

    hooks:
      protect-files:
        extra-patterns:
          - \"**/config/prod.yaml\"
          - \"~/.ssh/**\"
        disable-defaults: false

  Globs use NIO PathMatcher semantics ('**' crosses dirs, '*' does not).
  '~' at the start of an extra-pattern expands to $HOME.

  Fails closed: malformed YAML or an invalid glob produces a deny decision
  naming the problem, not a silent skip. Matches scope-lock's posture —
  better to block a legit edit than silently miss a sensitive one."
  (:require [cch.core :refer [defhook]]
            [cch.config :as config]
            [cch.protocol :as proto]
            [clojure.string :as str]
            [babashka.fs :as fs])
  (:import (java.nio.file FileSystems)))

(def default-patterns
  "Conservative built-in deny list. Extend via hooks.protect-files.extra-patterns
  rather than editing this — new patterns here ship to every user of cch."
  ["**/.env"               ".env"
   "**/.env.*"             ".env.*"
   "**/.git/**"            "**/.git"
   "**/*.pem"              "**/*.key"
   "**/id_rsa"             "**/id_rsa.pub"
   "**/id_ed25519"         "**/id_ed25519.pub"
   "**/.ssh/**"
   "**/.aws/credentials"
   "**/.netrc"
   "**/.npmrc"
   "**/secrets/**"
   "**/*.kdbx"             "**/*.keystore"])

(defn- expand-home
  "Expand a leading ~ to $HOME. Anything else returned unchanged."
  [pattern]
  (if (and pattern (str/starts-with? pattern "~"))
    (str (System/getProperty "user.home") (subs pattern 1))
    pattern))

(defn- compile-matcher
  "Compile a glob pattern into a PathMatcher. Throws IllegalArgumentException
  on invalid syntax — callers translate into a deny decision."
  [pattern]
  (.getPathMatcher (FileSystems/getDefault) (str "glob:" pattern)))

(defn- compile-patterns
  "Compile patterns to {:raw string :matcher PathMatcher}. Returns
  {:ok [...]} or {:error msg} on the first compile failure."
  [patterns]
  (try
    {:ok (mapv (fn [p]
                 (let [expanded (expand-home p)]
                   {:raw p :matcher (compile-matcher expanded)}))
               patterns)}
    (catch Exception e
      {:error (str "invalid glob pattern: " (.getMessage e))})))

(defn- match?
  "True if any compiled matcher matches the path or its filename component.
  Matching the filename alone lets bare-name globs like '.env' fire on any
  path ending in '.env' without forcing users to write '**/.env'."
  [compiled file-path]
  (let [full     (fs/path file-path)
        filename (fs/file-name full)]
    (some (fn [{:keys [matcher raw]}]
            (when (or (.matches matcher full)
                      (.matches matcher (fs/path filename)))
              raw))
          compiled)))

(defn check-path
  "Pure logic: return nil (allow) or a deny decision map.

  file-path — path from tool_input; may be nil
  patterns  — seq of glob strings (already-merged defaults + extras)

  Does not do I/O. Callers handle config loading and pass the merged list."
  [file-path patterns]
  (when (and file-path (seq patterns))
    (let [compiled (compile-patterns patterns)]
      (if (:error compiled)
        {:decision :deny
         :reason   (str "protect-files: " (:error compiled))}
        (when-let [hit (match? (:ok compiled) file-path)]
          {:decision :deny
           :reason   (str "protect-files: blocked edit to sensitive path\n"
                          "  file:    " file-path "\n"
                          "  pattern: " hit)})))))

(defn effective-patterns
  "Merge built-in defaults with user-supplied extras.
  When :disable-defaults is truthy, returns only the extras."
  [hook-cfg]
  (let [extras   (:extra-patterns hook-cfg)
        disable? (:disable-defaults hook-cfg)]
    (concat (when-not disable? default-patterns) extras)))

(defhook protect-files
  "Hard-deny edits to well-known secret/credential paths."
  {}
  [input]
  (let [file-path   (proto/extract-file-path input)
        cwd         (:cwd input)
        ;; Config lookup intentionally doesn't require a git repo — this
        ;; hook must work when editing into ~/.ssh/ from any cwd.
        config-path (config/find-config-up (or cwd ".") ".cch-config.yaml")
        cfg         (try
                      (config/load-yaml config-path)
                      (catch clojure.lang.ExceptionInfo _e
                        ::malformed))]
    (if (= cfg ::malformed)
      {:decision :deny
       :reason   (str "protect-files: malformed config at " config-path
                      " — refusing to load (fail closed)")}
      (check-path file-path (effective-patterns (get-in cfg [:hooks :protect-files]))))))
