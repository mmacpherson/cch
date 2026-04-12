(ns cli.cch
  "cch — Claude Code Hooks CLI.

  Usage: cch <command> [args]

  Commands:
    init              Set up cch in the current project
    install <hook>    Enable a hook (project-local by default)
    uninstall <hook>  Disable a hook
    list              Show available and installed hooks
    log               Query event history
    serve             Run the HTTP dispatcher + web dashboard"
  (:require [cch.server :as server]
            [cli.init :as init]
            [cli.install :as install]
            [cli.list-cmd :as list-cmd]
            [cli.log-cmd :as log-cmd]))

(defn print-usage []
  (println "cch — Claude Code Hooks")
  (println)
  (println "Usage: cch <command> [args]")
  (println)
  (println "Commands:")
  (println "  init              Set up cch in the current project")
  (println "  install <hook>    Enable a hook (--global for all projects, --http for HTTP mode)")
  (println "  uninstall <hook>  Disable a hook")
  (println "  list              Show available and installed hooks")
  (println "  log               Query event history")
  (println "  serve             Run the HTTP dispatcher + web dashboard")
  (println)
  (println "Run 'cch <command> --help' for details."))

(defn -main [& args]
  (let [[cmd & rest-args] args]
    (case cmd
      "init"      (apply init/run rest-args)
      "install"   (apply install/run rest-args)
      "uninstall" (apply install/run-uninstall rest-args)
      "list"      (apply list-cmd/run rest-args)
      "log"       (apply log-cmd/run rest-args)
      "serve"     (apply server/-main rest-args)
      (print-usage))))
