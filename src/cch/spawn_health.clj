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
   :argv ["/bin/sh" "-c" "exit 0"]})

(defonce ^:private state
  (atom {:consecutive-failures 0 :last-error nil :unhealthy? false}))

(defn next-state
  "Fold one probe result into the state. result is :ok or an error class
  name; only spawn errors count, a child's exit status never does. Pure."
  [current result threshold]
  (if (= :ok result)
    (assoc current :consecutive-failures 0 :unhealthy? false)
    (let [n (inc (:consecutive-failures current 0))]
      (assoc current :consecutive-failures n :last-error result
             :unhealthy? (>= n threshold)))))

(defn probe
  "Start argv and wait for it. Return :ok, or the simple class name of the
  error that stopped the child from starting."
  [argv]
  (try
    (let [p (-> (ProcessBuilder. ^java.util.List (vec argv))
                (.redirectErrorStream true)
                .start)]
      (.close (.getOutputStream p))
      (.readAllBytes (.getInputStream p))
      (.waitFor p)
      :ok)
    (catch java.io.IOException e
      (.getSimpleName (class e)))))

(defn health
  "Snapshot for /health."
  []
  (let [{:keys [consecutive-failures last-error unhealthy?]} @state]
    (cond-> {:ok (not unhealthy?) :consecutive-failures consecutive-failures}
      last-error (assoc :last-error last-error))))

(defn check!
  "Run one probe and fold it into the state. Returns the new state."
  [{:keys [argv threshold]}]
  (swap! state next-state (probe argv) threshold))

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
