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

(defn hook-http-url
  "Generate the HTTP dispatch URL for a hook. Default server is
  127.0.0.1:8888; override by passing opts."
  [hook-ns & {:keys [host port] :or {host "127.0.0.1" port 8888}}]
  (let [hook-name (last (str/split (str hook-ns) #"\."))]
    (format "http://%s:%d/hooks/%s" host port hook-name)))

(defn- cch-tagged?
  "Returns true if a hook entry is cch's: either a tagged command or an
  HTTP entry whose URL points at a cch dispatcher route for this hook."
  [hook-cmd hook-name]
  (let [cmd (:command hook-cmd)
        url (:url hook-cmd)]
    (cond
      cmd (str/includes? cmd (str "# cch:" hook-name))
      url (str/includes? url (str "/hooks/" hook-name))
      :else false)))

(defn- strip-cch-command
  "Remove only the cch-tagged command from an entry, preserving others.
  Returns the updated entry, or nil if no hooks remain."
  [entry hook-name]
  (let [kept (vec (remove #(cch-tagged? % hook-name) (:hooks entry)))]
    (when (seq kept)
      (assoc entry :hooks kept))))

(defn add-hook!
  "Add a hook to a settings file. Returns the updated settings.
  Surgically removes only cch's prior entry for this hook, preserving
  co-located non-cch hooks.

  mode is :command (default) or :http. HTTP entries point at
  http://127.0.0.1:8888/hooks/<hook-name>; host/port are overridable via
  opts (:http-host, :http-port)."
  [settings-path event-type matcher hook-ns & {:keys [mode http-host http-port]
                                                :or {mode :command}}]
  (let [settings  (read-settings settings-path)
        hooks-key (keyword event-type)
        hooks-vec (get-in settings [:hooks hooks-key] [])
        hook-name (last (str/split (str hook-ns) #"\."))
        ;; Remove any prior cch entry (command- or HTTP-tagged), preserving
        ;; co-located non-cch hooks in the same matcher group.
        filtered  (vec (keep #(strip-cch-command % hook-name) hooks-vec))
        hook-map  (case mode
                    :command {:type "command" :command (hook-command hook-ns)}
                    :http    {:type "http"
                              :url     (hook-http-url hook-ns :host (or http-host "127.0.0.1")
                                                              :port (or http-port 8888))
                              :timeout 5})
        ;; Omit :matcher for events that don't support it — keeps settings.json tidy.
        entry     (cond-> {:hooks [hook-map]}
                    matcher (assoc :matcher matcher))
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
