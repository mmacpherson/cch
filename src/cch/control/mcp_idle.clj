(ns cch.control.mcp-idle
  "Idle watchdog for the per-session stdio MCP child.

  Codex's shared app-server spawns one `cch control mcp` stdio JVM per thread
  and, when the thread ends, does not close the child's stdin — the child
  never sees EOF, so the JVM would live for the whole app-server lifetime and
  accumulate one ~50-200MB process per dead thread. This watchdog exits the
  process after a bounded window with no inbound client traffic; the provider
  relaunches the required server on the next tool call. Claude Code closes its
  child's stdin on exit, so its children reap on EOF and only benefit from this
  as a backstop. See claude-code-hooks-9qz.

  Decision logic here is pure; the only side effect is the daemon thread that
  `start-watchdog!` launches, and its clock/sleep/exit are all injectable."
  (:require [clojure.string :as str]
            [plumcp.core.protocol :as p]))

(def default-timeout-ms
  "30 minutes. Long enough to outlast a human's pause mid-thread, short enough
  that a dead thread's child does not accumulate for days."
  (* 30 60 1000))

(def default-poll-ms
  "How often the watchdog re-checks idleness. Cheap; a coarse cadence keeps the
  sleeping thread near-free."
  30000)

(defn parse-timeout-ms
  "Resolve the idle timeout (ms) from an env value. Blank or absent yields the
  default; an explicit non-positive or unparseable value disables the watchdog
  (nil). Pure."
  [env-value]
  (if (str/blank? env-value)
    default-timeout-ms
    (let [n (parse-long (str/trim env-value))]
      (when (and n (pos? n)) n))))

(defn idle-timeout-for
  "The idle timeout to apply for one caller kind. Only Codex needs it: its
  shared app-server keeps a dead thread's stdin open but relaunches the
  required server on the next tool call. Claude Code closes the child's stdin
  when the session exits, so its child reaps on EOF, and it does not relaunch
  an exited stdio server, so an idle exit would leave a live session without
  send_message until a manual /mcp reconnect. Pure."
  [caller timeout-ms]
  (when-not (= "claude" caller) timeout-ms))

(defn orphaned?
  "True when the process that launched this one has exited. Pure over its
  argument: parent-alive? is the result of checking the parent process."
  [parent-alive?]
  (not parent-alive?))

(defn start-orphan-watchdog!
  "Launch a daemon thread that invokes `on-orphaned` once `parent-alive?`
  reports the launching process gone, then stops. A backstop for any provider
  that exits without closing the child's stdin."
  [{:keys [parent-alive? poll-ms sleep-fn on-orphaned]
    :or {poll-ms default-poll-ms
         sleep-fn (fn [ms] (Thread/sleep (long ms)))}}]
  (let [worker (fn []
                 (loop []
                   (if (orphaned? (parent-alive?))
                     (on-orphaned)
                     (do (sleep-fn poll-ms) (recur)))))]
    (doto (Thread. ^Runnable worker "cch-mcp-orphan-watchdog")
      (.setDaemon true)
      (.start))))

(defn parent-alive-fn
  "Return a 0-arg fn reporting whether this JVM's parent process still runs.
  When the parent cannot be determined it reports alive, so the watchdog
  never kills a child it cannot reason about."
  []
  (if-let [parent (.orElse (.parent (java.lang.ProcessHandle/current)) nil)]
    #(.isAlive ^java.lang.ProcessHandle parent)
    (constantly true)))

(defn idle-expired?
  "True when at least timeout-ms has elapsed since last-activity-ms. A nil
  timeout (disabled) never expires. Pure."
  [timeout-ms last-activity-ms now-ms]
  (boolean (and timeout-ms (>= (- now-ms last-activity-ms) timeout-ms))))

(defn activity-logger
  "Wrap an ITrafficLogger so every inbound client message stamps `activity!`
  with the current time, while delegating all logging to `inner` unchanged.
  `activity!` is a 0-arg side-effecting fn (e.g. #(reset! a (now)))."
  [inner activity!]
  (reify p/ITrafficLogger
    (log-http-request [_ x] (activity!) (p/log-http-request inner x))
    (log-http-response [_ x] (p/log-http-response inner x))
    (log-http-failure [_ x] (p/log-http-failure inner x))
    (log-incoming-jsonrpc-request [_ x] (activity!) (p/log-incoming-jsonrpc-request inner x))
    (log-outgoing-jsonrpc-request [_ x] (p/log-outgoing-jsonrpc-request inner x))
    (log-incoming-jsonrpc-success [_ id r] (activity!) (p/log-incoming-jsonrpc-success inner id r))
    (log-outgoing-jsonrpc-success [_ id r] (p/log-outgoing-jsonrpc-success inner id r))
    (log-incoming-jsonrpc-failure [_ id e] (activity!) (p/log-incoming-jsonrpc-failure inner id e))
    (log-outgoing-jsonrpc-failure [_ id e] (p/log-outgoing-jsonrpc-failure inner id e))
    (log-incoming-jsonrpc-notification [_ x] (activity!) (p/log-incoming-jsonrpc-notification inner x))
    (log-outgoing-jsonrpc-notification [_ x] (p/log-outgoing-jsonrpc-notification inner x))
    (log-mcpcall-failure [_ x] (p/log-mcpcall-failure inner x))
    (log-mcp-sse-message [_ x] (p/log-mcp-sse-message inner x))))

(defn start-watchdog!
  "Launch a daemon thread that invokes `on-idle` once the connection has been
  idle for `timeout-ms`, then stops. Returns the Thread (already started), or
  nil when `timeout-ms` is nil (disabled).

  `activity` is an atom holding the last-activity epoch-ms (touched by
  `activity-logger`). `now-fn`, `sleep-fn`, and `on-idle` are injectable so the
  loop can be driven deterministically in tests."
  [{:keys [activity timeout-ms poll-ms now-fn sleep-fn on-idle]
    :or {poll-ms default-poll-ms
         now-fn (fn [] (System/currentTimeMillis))
         sleep-fn (fn [ms] (Thread/sleep (long ms)))}}]
  (when timeout-ms
    (let [worker (fn []
                   (loop []
                     (if (idle-expired? timeout-ms @activity (now-fn))
                       (on-idle)
                       (do (sleep-fn poll-ms) (recur)))))
          thread (doto (Thread. ^Runnable worker "cch-mcp-idle-watchdog")
                   (.setDaemon true)
                   (.start))]
      thread)))
