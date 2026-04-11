(ns cli.init
  "cch init — set up cch in the current project."
  (:require [cch.log :as log]
            [cch.config :as config]
            [babashka.fs :as fs]))

(defn run [& _args]
  (println "Initializing cch...")

  ;; Ensure global config exists
  (let [global-path (config/global-config-path)]
    (if (fs/exists? global-path)
      (println "  Global config exists:" global-path)
      (do
        (fs/create-dirs (fs/parent global-path))
        (spit global-path "{:log {:enabled true}}\n")
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
        (spit project-config ";; cch project configuration\n;; See: https://github.com/user/claude-code-hooks\n{}\n")
        (println "  Created project config:" project-config))))

  ;; Ensure .claude directory exists
  (when-not (fs/exists? ".claude")
    (fs/create-dirs ".claude"))

  (println)
  (println "Done! Next steps:")
  (println "  cch list              — see available hooks")
  (println "  cch install scope-lock — enable file scope enforcement"))
