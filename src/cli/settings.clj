(ns cli.settings
  "Atomic read/modify/write of Claude Code settings.json files."
  (:require [cheshire.core :as json]
            [babashka.fs :as fs]
            [clojure.string :as str]))

(defn global-settings-path
  "Path to the global Claude Code settings.json."
  []
  (str (System/getProperty "user.home") "/.claude/settings.json"))

(defn project-settings-path
  "Path to the project-level settings.local.json.
  If cwd is nil, returns path relative to current directory."
  [cwd]
  (str (or cwd ".") "/.claude/settings.local.json"))

(defn read-settings
  "Read and parse a settings JSON file. Returns {} if not found."
  [path]
  (if (fs/exists? path)
    (json/parse-string (slurp path) true)
    {}))

(defn write-settings!
  "Write settings atomically (write to tmp, rename)."
  [path data]
  (let [dir (fs/parent path)]
    (when-not (fs/exists? dir)
      (fs/create-dirs dir))
    (let [tmp (str path ".tmp")]
      (spit tmp (json/generate-string data {:pretty true}))
      (fs/move tmp path {:replace-existing true}))))

(defn cch-repo-root
  "Path to the cch framework repo (via symlink at ~/.local/share/cch/repo)."
  []
  (let [xdg (or (System/getenv "XDG_DATA_HOME")
                (str (System/getProperty "user.home") "/.local/share"))]
    (str xdg "/cch/repo")))

(defn hook-command
  "Generate the hook command string for a given hook namespace.
  Project-local path comes first so it can override built-in hooks.
  Includes resources for schema.sql access in logging."
  [hook-ns]
  (let [repo (cch-repo-root)]
    (str "bb -cp \"$CLAUDE_PROJECT_DIR/.claude/hooks/src"
         ":" repo "/src"
         ":" repo "/resources\""
         " -m " hook-ns
         " # cch:" (last (str/split (str hook-ns) #"\.")))))

(defn- cch-tagged?
  "Returns true if a hook command has the given cch tag."
  [hook-cmd hook-name]
  (and (:command hook-cmd)
       (str/includes? (:command hook-cmd) (str "# cch:" hook-name))))

(defn- strip-cch-command
  "Remove only the cch-tagged command from an entry, preserving others.
  Returns the updated entry, or nil if no hooks remain."
  [entry hook-name]
  (let [kept (vec (remove #(cch-tagged? % hook-name) (:hooks entry)))]
    (when (seq kept)
      (assoc entry :hooks kept))))

(defn add-hook!
  "Add a hook to a settings file. Returns the updated settings.
  Surgically removes only the cch-tagged command, preserving co-located hooks."
  [settings-path event-type matcher hook-ns]
  (let [settings  (read-settings settings-path)
        hooks-key (keyword event-type)
        hooks-vec (get-in settings [:hooks hooks-key] [])
        hook-name (last (str/split (str hook-ns) #"\."))
        command   (hook-command hook-ns)
        ;; Remove only the cch command, preserving other hooks in same entry
        filtered  (vec (keep #(strip-cch-command % hook-name) hooks-vec))
        entry     {:matcher matcher
                   :hooks   [{:type "command" :command command}]}
        updated   (conj filtered entry)
        new-settings (assoc-in settings [:hooks hooks-key] updated)]
    (write-settings! settings-path new-settings)
    new-settings))

(defn remove-hook!
  "Remove a hook from a settings file by its cch tag name.
  Operates at the command level — preserves co-located non-cch hooks."
  [settings-path event-type hook-name]
  (let [settings  (read-settings settings-path)
        hooks-key (keyword event-type)
        hooks-vec (get-in settings [:hooks hooks-key] [])
        filtered  (vec (keep #(strip-cch-command % hook-name) hooks-vec))
        new-settings (assoc-in settings [:hooks hooks-key] filtered)]
    (write-settings! settings-path new-settings)
    new-settings))
