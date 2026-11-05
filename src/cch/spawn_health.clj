(ns cch.spawn-health
  "Watch whether this JVM can still start child processes.

  A long-lived JVM can lose that ability when the Java runtime it was started
  from is replaced on disk (an in-place JDK upgrade, or a runtime redeploy):
  each spawn then fails with an IOException such as 'posix_spawn failed',
  while HTTP keeps serving and every hook that shells out to git fails
  quietly. Only a fresh JVM recovers. A daemon thread starts a trivial child
  on an interval; after several consecutive spawn failures it reports the
  JVM unhealthy and calls on-unhealthy, which for the service is an exit so
  systemd or launchd starts a fresh process.")

(def ^:private defaults
  {:interval-ms 60000
   :threshold 3
   :argv ["/bin/sh" "-c" "exit 0"]
   :probe-timeout-ms 10000})

(defonce ^:private state
  (atom {:consecutive-failures 0 :last-error nil :unhealthy? false}))

(defn next-state
  "Fold one probe result into the state. result is :ok or a failure name
  (a spawn error class, or ProbeTimeout); a child's exit status never
  counts. Pure."
  [current result threshold]
  (if (= :ok result)
    (assoc current :consecutive-failures 0 :unhealthy? false)
    (let [n (inc (:consecutive-failures current 0))]
      (assoc current :consecutive-failures n :last-error result
             :unhealthy? (>= n threshold)))))

(defn probe
  "Start argv and wait up to timeout-ms for it. Return :ok, or why it failed:
  the simple class name of the error that stopped the child from starting, or
  \"ProbeTimeout\" when a trivial child never exits, so a hung helper cannot
  silently disable the watchdog."
  ([argv] (probe argv 10000))
  ([argv timeout-ms]
   (try
     (let [p (-> (ProcessBuilder. ^java.util.List (vec argv))
                 (.redirectOutput java.lang.ProcessBuilder$Redirect/DISCARD)
                 (.redirectError java.lang.ProcessBuilder$Redirect/DISCARD)
                 .start)]
       (.close (.getOutputStream p))
       (if (.waitFor p (long timeout-ms) java.util.concurrent.TimeUnit/MILLISECONDS)
         :ok
         (do (.destroyForcibly p) "ProbeTimeout")))
     (catch java.io.IOException e
       (.getSimpleName (class e))))))

(defn health
  "Snapshot for /health."
  []
  (let [{:keys [consecutive-failures last-error unhealthy?]} @state]
    (cond-> {:ok (not unhealthy?) :consecutive-failures consecutive-failures}
      last-error (assoc :last-error last-error))))

(defn check!
  "Run one probe and fold it into the state. Returns the new state."
  [{:keys [argv threshold probe-timeout-ms]}]
  (swap! state next-state (probe argv probe-timeout-ms) threshold))

(defn exit-for-restart!
  "Exit so the service manager starts a fresh JVM. Shutdown hooks get
  grace-ms; if they hang, halt anyway so recovery cannot stall."
  ([status] (exit-for-restart! status 30000))
  ([status grace-ms]
   (doto (Thread. (fn []
                    (try (Thread/sleep (long grace-ms))
                         (catch InterruptedException _ nil))
                    (.halt (Runtime/getRuntime) status))
                  "cch-spawn-health-halt")
     (.setDaemon true)
     .start)
   (System/exit status)))

(defonce ^:private worker (atom nil))

(defn start!
  "Start the watchdog. on-unhealthy is called once, with the state, when
  consecutive spawn failures reach the threshold."
  [{:keys [on-unhealthy] :as opts}]
  (when-not @worker
    (let [{:keys [interval-ms] :as opts} (merge defaults opts)
          running (atom true)
          t (Thread.
              (fn []
                (while @running
                  (try
                    (Thread/sleep (long interval-ms))
                    (let [{:keys [unhealthy?] :as s} (check! opts)]
                      (when unhealthy?
                        (println (format (str "cch: %d consecutive child-process spawn failures (%s);"
                                              " this JVM can no longer run hooks that shell out")
                                         (:consecutive-failures s) (:last-error s)))
                        (reset! running false)
                        (when on-unhealthy (on-unhealthy s))))
                    (catch InterruptedException _ (reset! running false))
                    (catch Throwable e
                      (println "cch: spawn watchdog error:" (.getSimpleName (class e)))))))
              "cch-spawn-health")]
      (.setDaemon t true)
      (.start t)
      (reset! worker {:running running :thread t}))))

(defn stop! []
  (when-let [{:keys [running ^Thread thread]} @worker]
    (reset! running false)
    (.interrupt thread)
    (reset! worker nil)))
