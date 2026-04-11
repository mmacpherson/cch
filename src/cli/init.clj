(ns cli.init
  "cch init — set up cch in the current project."
  (:require [cch.log :as log]
            [cch.config :as config]
            [babashka.fs :as fs]
            [clojure.java.io :as io]
            [clojure.string :as str]))

(defn detect-repo-root
  "Detect the cch repo root from a known resource on the classpath.
  schema.sql is in resources/ at the repo root, so we can find it
  via io/resource and walk up to the repo root. This works regardless
  of the process's cwd."
  []
  (when-let [schema-url (io/resource "schema.sql")]
    (let [schema-path (str schema-url)]
      ;; file:/path/to/repo/resources/schema.sql → /path/to/repo
      (when (str/starts-with? schema-path "file:")
        (let [file-path (subs schema-path 5)]
          (str (fs/parent (fs/parent file-path))))))))

(defn ensure-repo-symlink!
  "Create ~/.local/share/cch/repo → the cch repo checkout."
  [repo-root]
  (let [xdg  (or (System/getenv "XDG_DATA_HOME")
                  (str (System/getProperty "user.home") "/.local/share"))
        link (str xdg "/cch/repo")]
    (fs/create-dirs (str xdg "/cch"))
    (if (fs/exists? link)
      (println "  Repo symlink exists:" link "→" (str (fs/read-link link)))
      (do
        (fs/create-sym-link link repo-root)
        (println "  Created repo symlink:" link "→" repo-root)))))

(defn run [& _args]
  (println "Initializing cch...")

  ;; Detect and link repo
  (if-let [repo-root (detect-repo-root)]
    (ensure-repo-symlink! repo-root)
    (println "  WARNING: could not detect cch repo root (not in a git repo)"))

  ;; Ensure global config exists
  (let [global-path (config/global-config-path)]
    (if (fs/exists? global-path)
      (println "  Global config exists:" global-path)
      (do
        (fs/create-dirs (fs/parent global-path))
        (spit global-path ";; cch global configuration\n;; Add overrides here as needed.\n{}\n")
        (println "  Created global config:" global-path))))

  ;; Ensure SQLite DB exists
  (let [db (log/db-path)]
    (log/ensure-db! db)
    (println "  Event log database:" db))

  ;; Create project config if not present
  (let [project-config ".claude-hooks.edn"]
    (if (fs/exists? project-config)
      (println "  Project config exists:" project-config)
      (do
        (spit project-config ";; cch project configuration\n;; See: https://github.com/mmacpherson/claude-code-hooks\n{}\n")
        (println "  Created project config:" project-config))))

  ;; Ensure .claude directory exists
  (when-not (fs/exists? ".claude")
    (fs/create-dirs ".claude"))

  (println)
  (println "Done! Next steps:")
  (println "  cch list              — see available hooks")
  (println "  cch install scope-lock — enable file scope enforcement"))
