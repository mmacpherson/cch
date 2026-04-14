(ns cli.settings
  "Atomic read/modify/write of Claude Code settings.json files.

  Three kinds of cch-owned entries now:

    1. Dispatch entries (one per event cch handles). type: http, URL ends
       with /dispatch/<event>. Every code hook's events go through these —
       Claude Code forwards all matching events to cch, which decides
       per-payload which registered hooks to run.

    2. Prompt entries (one per :type :prompt registry entry). type: prompt
       native Claude Code entry; Claude Code runs the LLM call itself.
       Tagged via a custom :__cch field since prompt entries have no URL
       to encode the tag in.

    3. Agent entries (one per :type :agent registry entry). type: agent
       native Claude Code entry. Also tagged via :__cch.

  All cch-owned entries are identifiable and surgically removable,
  preserving any non-cch hooks co-located under the same (event, matcher)
  group. Non-cch entries are never touched."
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

;; --- URL / tag helpers ---

(defn dispatch-url
  "URL the dispatcher listens on for a given event."
  [event & {:keys [host port] :or {host "127.0.0.1" port 8888}}]
  (format "http://%s:%d/dispatch/%s" host port event))

(defn- dispatch-entry-for-event?
  "True if a hook map (inside :hooks) is cch's dispatch entry for `event`.
  Matches URL path /dispatch/<event> regardless of host:port."
  [hook-map event]
  (when-let [url (:url hook-map)]
    (or (str/ends-with? url (str "/dispatch/" event))
        (str/includes? url (str "/dispatch/" event "?"))
        (str/includes? url (str "/dispatch/" event "#")))))

(defn- cch-owned?
  "True if a hook map is any kind of cch-owned entry (dispatch, prompt, agent)."
  [hook-map]
  (or (and (:url hook-map) (str/includes? (:url hook-map) "/dispatch/"))
      (some? (:__cch hook-map))))

;; --- Dispatch entries (per event) ---

(defn- strip-cch-dispatch
  "From an entry, remove any dispatch-hook map for `event`. Returns updated
  entry (possibly nil if no hooks remain)."
  [entry event]
  (let [kept (vec (remove #(dispatch-entry-for-event? % event) (:hooks entry)))]
    (when (seq kept)
      (assoc entry :hooks kept))))

(defn add-dispatch-entry!
  "Write (or replace) the universal dispatch entry for `event` in settings.
  Optionally set :matcher for tool events.

  Preserves co-located non-cch hooks. Removes any prior cch dispatch for
  the same event before adding the new one."
  [settings-path event & {:keys [matcher host port timeout]
                          :or   {host "127.0.0.1" port 8888 timeout 30}}]
  (let [settings (read-settings settings-path)
        hooks-key (keyword event)
        hooks-vec (get-in settings [:hooks hooks-key] [])
        filtered  (vec (keep #(strip-cch-dispatch % event) hooks-vec))
        hook-map  {:type    "http"
                   :url     (dispatch-url event :host host :port port)
                   :timeout timeout}
        entry     (cond-> {:hooks [hook-map]}
                    matcher (assoc :matcher matcher))
        updated   (conj filtered entry)
        new-settings (assoc-in settings [:hooks hooks-key] updated)]
    (write-settings! settings-path new-settings)
    new-settings))

(defn remove-dispatch-entry!
  "Remove the cch dispatch entry for a given event. Preserves non-cch hooks."
  [settings-path event]
  (let [settings (read-settings settings-path)
        hooks-key (keyword event)
        hooks-vec (get-in settings [:hooks hooks-key] [])
        filtered  (vec (keep #(strip-cch-dispatch % event) hooks-vec))
        new-settings (assoc-in settings [:hooks hooks-key] filtered)]
    (write-settings! settings-path new-settings)
    new-settings))

;; --- Prompt / agent entries (per hook) ---

(defn- hook-entry-tag?
  "True if a hook map is tagged for the given hook-name via :__cch."
  [hook-map tag-prefix hook-name]
  (= (:__cch hook-map) (str tag-prefix ":" hook-name)))

(defn- strip-cch-hook-entry
  "Remove any cch hook-entry matching (tag-prefix, hook-name) from an entry."
  [entry tag-prefix hook-name]
  (let [kept (vec (remove #(hook-entry-tag? % tag-prefix hook-name) (:hooks entry)))]
    (when (seq kept)
      (assoc entry :hooks kept))))

(defn- upsert-hook-entry!
  "Shared upsert for prompt/agent entries. `hook-map` is the rendered
  settings.json hook map (with :type, :__cch, plus fields per type)."
  [settings-path event matcher tag-prefix hook-name hook-map]
  (let [settings  (read-settings settings-path)
        hooks-key (keyword event)
        hooks-vec (get-in settings [:hooks hooks-key] [])
        filtered  (vec (keep #(strip-cch-hook-entry % tag-prefix hook-name) hooks-vec))
        entry     (cond-> {:hooks [hook-map]}
                    matcher (assoc :matcher matcher))
        updated   (conj filtered entry)
        new-settings (assoc-in settings [:hooks hooks-key] updated)]
    (write-settings! settings-path new-settings)
    new-settings))

(defn add-prompt-entry!
  "Write a native Claude Code prompt-type hook entry.
  Tagged via :__cch 'prompt:<hook-name>' so we can surgically remove it."
  [settings-path event matcher hook-name
   {:keys [prompt-template model timeout status-message if]
    :or   {timeout 30}}]
  (let [hook-map (cond-> {:type    "prompt"
                          :prompt  prompt-template
                          :timeout timeout
                          :__cch   (str "prompt:" hook-name)}
                   model          (assoc :model model)
                   status-message (assoc :statusMessage status-message)
                   if             (assoc :if if))]
    (upsert-hook-entry! settings-path event matcher "prompt" hook-name hook-map)))

(defn add-agent-entry!
  "Write a native Claude Code agent-type hook entry.
  Tagged via :__cch 'agent:<hook-name>'."
  [settings-path event matcher hook-name
   {:keys [agent-spec timeout status-message if]
    :or   {timeout 60}}]
  (let [hook-map (cond-> (merge {:type "agent" :timeout timeout
                                 :__cch (str "agent:" hook-name)}
                                agent-spec)
                   status-message (assoc :statusMessage status-message)
                   if             (assoc :if if))]
    (upsert-hook-entry! settings-path event matcher "agent" hook-name hook-map)))

(defn remove-hook-entry!
  "Remove a prompt or agent cch-owned entry by hook-name. Scans all event types
  since the caller may not remember which event it was installed under."
  [settings-path hook-name]
  (let [settings  (read-settings settings-path)
        event-types (keys (:hooks settings))
        updated   (reduce
                    (fn [s et]
                      (let [key (keyword (name et))
                            hooks-vec (get-in s [:hooks key] [])
                            filtered  (vec (keep (fn [entry]
                                                   (or (strip-cch-hook-entry entry "prompt" hook-name)
                                                       (strip-cch-hook-entry entry "agent" hook-name)
                                                       ;; neither applied — keep as-is
                                                       entry))
                                                 hooks-vec))]
                        (assoc-in s [:hooks key] filtered)))
                    settings
                    event-types)]
    (write-settings! settings-path updated)
    updated))

;; --- Full cleanup ---

(defn remove-all-cch!
  "Remove every cch-owned entry from settings.json (dispatch, prompt, agent).
  Preserves all non-cch hooks. Used by `cch uninstall` with no args."
  [settings-path]
  (let [settings  (read-settings settings-path)
        event-types (keys (:hooks settings))
        updated   (reduce
                    (fn [s et]
                      (let [key (keyword (name et))
                            hooks-vec (get-in s [:hooks key] [])
                            pruned (vec
                                     (keep (fn [entry]
                                             (let [kept (vec (remove cch-owned? (:hooks entry)))]
                                               (when (seq kept)
                                                 (assoc entry :hooks kept))))
                                           hooks-vec))]
                        (assoc-in s [:hooks key] pruned)))
                    settings
                    event-types)]
    (write-settings! settings-path updated)
    updated))
