(ns cch.attention
  "How much agent time is spent blocked on the user, derived from the
  existing event log.

  No new instrumentation: every signal needed is already recorded, so
  this reaches back to the first logged event rather than to whenever
  it was switched on.

  Two facts about the event table shape drive the queries below.

  Rows are per hook invocation, not per agent event. Three hooks
  matching `PreToolUse` on Bash produce three rows for one tool call.
  `event-log` is the observer registered for every event, so filtering
  on it yields exactly one row per real event — 640k of 1.24M.

  Blocking is bounded by the following event. An agent that raises a
  prompt is blocked until it does anything else, so the gap between a
  prompt and the next row in the same session is the wait."
  (:require [cch.db :as db]
            [clojure.string :as str]))

(def ^:const walked-away-secs
  "Longest gap counted as waiting, in seconds.

  Beyond this the user has left rather than hesitated, and including it
  would let one abandoned overnight session dominate the total. Capping
  rather than discarding keeps the episode counted at its plausible
  maximum."
  1800)

(def ^:private blocking-predicate
  "Which rows represent an agent waiting on a human.

  Claude Code raises `Notification`, subtyped: `permission_prompt` is a
  decision the user must make, `idle_prompt` is a finished turn awaiting
  the next instruction. They are different costs and are reported apart
  — only the first is reducible by granting permissions.

  Codex has no equivalent subtype; its `PermissionRequest` is always a
  decision, which is why it is labelled to match."
  "((agent='claude-code' and event_type='Notification'
      and kind in ('permission_prompt','idle_prompt'))
    or (agent='codex' and event_type='PermissionRequest'))")

(defn- episodes-cte
  "SQL defining one row per blocking episode with its bounded duration."
  [since-clause]
  (format "
with e as (
  select session_id, agent, cwd, event_type,
         coalesce(json_extract(extra,'$.notification_type'),'permission_request') kind,
         timestamp,
         lead(timestamp) over (partition by session_id order by id) nxt
  from events
  where hook_name='event-log' %s
),
episodes as (
  select agent, kind, cwd, timestamp,
         min((julianday(nxt)-julianday(timestamp))*86400, %d) secs
  from e
  where nxt is not null and %s
)" since-clause walked-away-secs blocking-predicate))

(defn since-clause
  "Restrict to the last `days`, or all history when nil. Pure."
  [days]
  (if days
    (format "and timestamp >= date('now', '-%d days')" (long days))
    ""))

(defn by-kind
  "Blocking totals split by agent and kind, worst first."
  [days]
  (db/query
    (str (episodes-cte (since-clause days))
         " select agent, kind, count(*) episodes,
                  round(sum(secs)/3600.0,1) hours,
                  round(avg(secs)) avg_secs
           from episodes group by 1,2 order by hours desc")))

(defn by-project
  "Where decision-blocking concentrates. Excludes idle prompts, which
  measure the user thinking rather than a reducible cost."
  [days limit]
  (db/query
    (str (episodes-cte (since-clause days))
         (format " select cwd, count(*) episodes,
                          round(sum(secs)/3600.0,1) hours
                  from episodes
                  where kind != 'idle_prompt'
                  group by 1 order by hours desc limit %d" (long limit)))))

(defn- abbrev-home [s]
  (if (and s (str/starts-with? s (System/getProperty "user.home")))
    (str "~" (subs s (count (System/getProperty "user.home"))))
    (or s "")))

(defn render
  "Format a summary. Figures and their denominators, no narration. Pure —
  takes already-fetched rows so the layout is testable without a DB."
  [{:keys [kinds projects days limit] :or {limit 8}}]
  (let [window (if days (format "last %d days" (long days)) "all history")]
    (str/join
      "\n"
      (concat
        [(format "Agent time blocked on you — %s" window)
         ""
         (format "  %-13s %-19s %9s %8s %9s" "agent" "kind" "episodes" "hours" "avg")]
        (for [{:keys [agent kind episodes hours avg_secs]} kinds]
          (format "  %-13s %-19s %9d %8.1f %8.0fs" agent kind episodes hours (double avg_secs)))
        [""
         (format "Decision-blocking by project (top %d, idle prompts excluded)" limit)
         ""]
        (for [{:keys [cwd episodes hours]} projects]
          (format "  %-58s %5d %7.1fh" (abbrev-home cwd) episodes hours))
        [""
         (format "  Waits longer than %ds counted as %ds — beyond that the user has left."
                 walked-away-secs walked-away-secs)]))))

(defn report
  "Fetch and render. The IO half of `render`."
  [{:keys [days limit] :or {limit 8} :as opts}]
  (render (assoc opts
                 :kinds    (by-kind days)
                 :projects (by-project days limit)
                 :limit    limit)))
