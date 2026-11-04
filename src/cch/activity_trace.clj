(ns cch.activity-trace
  "Pure runner-local event reconstruction. Timestamps are UTC without a zone;
  preserve their spelling. Never include tool outputs in summaries or digests."
  (:require [cheshire.core :as json]
            [clojure.string :as str]))

(defn- object [x]
  (cond
    (map? x) x
    (string? x) (try (let [v (json/parse-string x true)] (if (map? v) v {}))
                     (catch Exception _ {}))
    :else {}))

(defn- line [x]
  (when (string? x)
    (let [s (str/trim (or (first (str/split-lines x)) ""))]
      (subs s 0 (min 160 (count s))))))

(defn- command-summary [command]
  (when (string? command)
    (let [[head & body] (str/split-lines command)]
      (if (and head (re-find #"<<-?\s*['\"]?[a-zA-Z_][a-zA-Z_0-9]*['\"]?\s*$" head))
        (str head " (heredoc)" (when-let [first-line (first (remove str/blank? body))]
                               (str " " (str/trim first-line))))
        head))))

(defn tool-summary
  "One safe structural line, at most 160 characters. Does not read tool output."
  [tool-name tool-input]
  (let [input (object tool-input)
        patch (or (:patch input) (:input input) (when (string? tool-input) tool-input))
        paths (when (string? patch)
                (map (fn [[_ path moved]] (or path moved)) (re-seq #"(?m)^\*\*\* (?:Add|Update|Delete) File: (.+)$|^\*\*\* Move to: (.+)$" patch)))
        paths (when paths (remove nil? paths))
        summary (case tool-name
                  ("Bash" "exec_command" "shell_command") (command-summary (or (:command input) (:cmd input)))
                  ("Read" "Edit" "Write") (or (:file_path input) (:path input))
                  ("Grep" "Glob") (:pattern input)
                  ("apply_patch" "ApplyPatch") (when (seq paths) (str/join ", " paths))
                  tool-name)]
    (or (not-empty (line summary)) (line tool-name) "Unknown tool")))

(defn- duration [start end]
  (try
    (let [ms (.toMillis (java.time.Duration/between
                         (java.time.LocalDateTime/parse start)
                         (java.time.LocalDateTime/parse end)))]
      (when (<= 0 ms) ms))
    (catch Exception _ nil)))

(defn- step [at kind summary outcome tool]
  {:at at :kind kind :summary (or (line summary) "") :outcome outcome
   :duration-ms nil :tool tool})

(defn- finish [turn at]
  (-> turn
      (assoc :ended-at at)
      (update :steps #(mapv (fn [s] (if (and (= :tool (:kind s)) (= :pending (:outcome s)))
                                     (assoc s :outcome :unknown) s)) %))))

(defn- new-turn [key at]
  {:turn-key key :started-at at :ended-at nil :prompts [] :steps []
   :closing-message nil ::pending {}})

(defn- event-step [event row payload]
  (let [at (:timestamp row) tool (or (:tool_name payload) (:tool_name row))]
    (cond
      (#{"PermissionRequest" "PermissionDenied"} event)
      (step at :permission (str event ": " (tool-summary tool (or (:tool_input payload) (:tool_params payload))))
            (if (= "PermissionDenied" event) :denied :asked) tool)
      (#{"PreCompact" "PostCompact"} event)
      (step at :compaction (str event (when-let [trigger (:trigger payload)] (str ": " trigger))) :ok nil)
      (#{"SubagentStart" "SubagentStop"} event)
      (step at :subagent (str event (when-let [name (or (:agent_type payload) (:agent_id payload))] (str ": " name))) :ok nil)
      (= "CwdChanged" event) (step at :cwd (or (:cwd payload) (:cwd row) "Working directory changed") :ok nil)
      :else (step at :notification (or (:message payload) event "Unknown event") :ok nil))))

(defn- apply-event [turn row payload event]
  (let [at (:timestamp row)
        tool (or (:tool_name payload) (:tool_name row))
        tool-id (:tool_use_id payload)
        summary (tool-summary tool (or (:tool_input payload) (:tool_params payload) (when-let [path (:file_path row)] {:file_path path})))
        idx (when tool-id (get (::pending turn) tool-id))]
    (cond
      (not= "event-log" (:hook_name row))
      (update turn :steps conj (step at :hook-decision
                                    (str (:hook_name row) (when-let [reason (:reason row)] (str ": " reason)))
                                    (if (= "deny" (:decision row)) :denied :asked) tool))
      (= "UserPromptSubmit" event)
      (update turn :prompts conj {:at at :text (if (string? (:prompt payload)) (:prompt payload) "")})
      (= "PreToolUse" event)
      (cond-> (update turn :steps conj (step at :tool summary :pending tool))
        tool-id (assoc-in [::pending tool-id] (count (:steps turn))))
      (#{"PostToolUse" "PostToolUseFailure"} event)
      (let [outcome (if (or (= "PostToolUseFailure" event)
                            (true? (:is_error payload))
                            (true? (get-in payload [:tool_response :is_error]))
                            (true? (get-in payload [:tool_response :isError]))
                            (some #(and (number? %) (not (zero? %)))
                                  [(:exit_code payload) (get-in payload [:tool_response :exit_code])])) :failed :ok)
            ms (or (when (number? (:duration_ms payload)) (:duration_ms payload))
                   (when (some? idx) (duration (get-in turn [:steps idx :at]) at)))
            completed (if (some? idx)
                        (update-in turn [:steps idx] assoc :outcome outcome :duration-ms ms)
                        (update turn :steps conj (assoc (step at :tool summary outcome tool) :duration-ms ms)))]
        (update completed ::pending dissoc tool-id))
      (= "PermissionDenied" event)
      (cond-> (update turn :steps conj (event-step event row payload))
        (some? idx) (assoc-in [:steps idx :outcome] :denied))
      (#{"Stop" "StopFailure" "SessionEnd"} event)
      (cond-> (finish turn at)
        (string? (:last_assistant_message payload)) (assoc :closing-message (:last_assistant_message payload)))
      :else (update turn :steps conj (event-step event row payload)))))

(defn- dedupe-tools [rows]
  ;; Duplicate dispatcher registrations can report a single invocation twice.
  ;; Tool ids identify calls; never collapse distinct calls with equal commands,
  ;; or repeated prompts (which may be intentional steering).
  (:rows
    (reduce (fn [{:keys [seen] :as state} row]
              (let [p (object (:extra row))
                    event (or (:event_type row) (:hook_event_name p))
                    key [(:agent row) (:session_id row) event (:tool_use_id p)
                         (or (:turn_id p) (:prompt_id p))]
                    previous (get seen key)
                    delta (when previous (duration (:timestamp previous) (:timestamp row)))
                    duplicate? (and previous
                                    (or (= (::payload previous) p)
                                        (and delta (<= delta 1000))))]
                (if (and (= "event-log" (:hook_name row))
                         (:tool_use_id p)
                         (#{"PreToolUse" "PostToolUse" "PostToolUseFailure"} event))
                  (if duplicate? state
                    (-> state
                        (assoc-in [:seen key] (assoc row ::payload p))
                        (update :rows conj row)))
                  (update state :rows conj row))))
            {:rows [] :seen {}} rows)))

(defn turns
  "Reconstruct oldest-first event rows into turns. Other hooks contribute only
  deny/ask decisions. Missing ids get deterministic synthetic keys. A new turn
  closes an unfinished preceding turn at the new boundary. Late rows bearing
  explicit turn ids attach to their original turn. Unmatched completed tools
  remain visible; unmatched starts are pending, or unknown after turn end."
  [rows]
  (mapv (fn [turn] (dissoc (if (:ended-at turn) (finish turn (:ended-at turn)) turn) ::pending))
    (:turns (reduce
      (fn [{:keys [turns current keyed cwd tool-turns] :as state} [ordinal row]]
        (if-not (or (= "event-log" (:hook_name row)) (#{"deny" "ask"} (:decision row)))
          state
          (let [payload (object (:extra row))
                event (or (:event_type row) (:hook_event_name payload))
                session [(:agent row) (:session_id row)]
                explicit (or (:turn_id payload) (:prompt_id payload))
                explicit (when (and (string? explicit) (not (str/blank? explicit))) explicit)
                previous (get current session)
                existing (or (when explicit (get keyed [session explicit]))
                             (when (#{"PostToolUse" "PostToolUseFailure" "PermissionDenied"} event)
                               (get tool-turns [session (:tool_use_id payload)])))
                fresh? (and (nil? existing)
                            (or (nil? previous) explicit
                                (= "UserPromptSubmit" event)
                                (some? (get-in turns [previous :ended-at]))))
                index (or existing (when-not fresh? previous) (count turns))
                at (:timestamp row)
                turns (if fresh?
                        (cond-> turns
                          (and (some? previous) (nil? (get-in turns [previous :ended-at])))
                          (update previous finish at)
                          true (conj (new-turn (or explicit (str "synthetic:" (:session_id row) ":" (or (:id row) ordinal))) at)))
                        turns)
                dir (or (:cwd payload) (:cwd row))
                changed? (and (string? dir) (not= dir (get cwd session)) (not= "CwdChanged" event))
                turns (cond-> turns changed? (update-in [index :steps] conj (step at :cwd dir :ok nil)))
                turns (update turns index apply-event row payload event)]
            (cond-> (assoc state :turns turns)
              (or fresh? (= index previous)) (assoc-in [:current session] index)
              explicit (assoc-in [:keyed [session explicit]] index)
              (and (= "event-log" (:hook_name row)) (= "PreToolUse" event) (:tool_use_id payload))
              (assoc-in [:tool-turns [session (:tool_use_id payload)]] index)
              (string? dir) (assoc-in [:cwd session] dir)))))
      {:turns [] :current {} :keyed {} :cwd {} :tool-turns {}}
      (map-indexed vector (dedupe-tools rows))))))

(defn digest-input
  "Allowlisted input for cch.scrub/digest. Includes all steered prompts in order;
  excludes timestamps, paths outside tool summaries, and raw provider payloads."
  [turn]
  {:prompt (str/join "\n" (map :text (:prompts turn)))
   :tool-summaries (mapv :summary (filter #(= :tool (:kind %)) (:steps turn)))
   :closing-message (:closing-message turn)})
