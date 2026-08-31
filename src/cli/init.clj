(ns cli.init
  "cch init — set up cch in the current project."
  (:require [cch.db :as db]
            [cch.log :as log]
            [cch.config :as config]
            [babashka.fs :as fs]
            [clojure.string :as str]))

(defn run [_options _arguments]
  (println "Initializing cch...")

  ;; Ensure global config exists
  (let [global-path (config/global-config-path)
        legacy-edn  (str/replace global-path #"\.yaml$" ".edn")]
    (cond
      (fs/exists? global-path)
      (println "  Global config exists:" global-path)

      (fs/exists? legacy-edn)
      (println "  Legacy global config found:" legacy-edn
               "\n    Rename it to" global-path "to adopt the new format.")

      :else
      (do
        (fs/create-dirs (fs/parent global-path))
        (spit global-path "# cch global configuration\n# Add overrides here as needed.\n")
        (println "  Created global config:" global-path))))

  ;; Ensure SQLite DB exists
  (let [path (db/db-path)]
    (log/ensure-db! path)
    (println "  Event log database:" path))

  ;; Create project config if not present
  (let [project-config ".cch-config.yaml"
        legacy-edn     ".claude-hooks.edn"]
    (cond
      (fs/exists? project-config)
      (println "  Project config exists:" project-config)

      (fs/exists? legacy-edn)
      (println "  Legacy project config found:" legacy-edn
               "\n    Rename and convert it to" project-config "to adopt the new format.")

      :else
      (do
        (spit project-config
              (str "# cch project configuration\n"
                   "# See: https://github.com/mmacpherson/cch\n"
                   "\n"
                   "# hooks:\n"
                   "#   scope-lock:\n"
                   "#     allowed-paths:\n"
                   "#       - src/\n"))
        (println "  Created project config:" project-config))))

  ;; Ensure .claude directory exists
  (when-not (fs/exists? ".claude")
    (fs/create-dirs ".claude"))

  (println)
  (println "Done! Next steps:")
  (println "  cch list              — see available hooks")
  (println "  cch install scope-lock — enable file scope enforcement"))
