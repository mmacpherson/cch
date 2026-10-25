(ns cch.subprocess
  "Small synchronous subprocess boundary for short-lived CLI integrations.

  Output streams are drained concurrently on virtual threads. Unlike future-
  backed helpers, these joined threads do not keep one-shot cch JVMs alive for
  an executor keepalive period after the command has completed."
  (:import [java.lang ProcessHandle]
           [java.nio.charset StandardCharsets]
           [java.util.concurrent TimeUnit]))

(defn- capture-stream! [stream destination error]
  (try
    (reset! destination (String. (.readAllBytes stream)
                                  StandardCharsets/UTF_8))
    (catch Exception cause
      (reset! error cause))))

(defn- kill-tree! [^Process process]
  (doseq [^ProcessHandle child (iterator-seq (.iterator (.descendants process)))]
    (.destroyForcibly child))
  (.destroyForcibly process)
  (.waitFor process))

(defn run
  "Run argv without a shell and return {:exit :out :err}.

  Stdin is closed immediately, so a command that unexpectedly prompts reads
  EOF instead of waiting forever. With :timeout-ms, a command that has not
  exited in time is killed with its descendants and an ex-info of type
  :subprocess-timeout is thrown; long-lived loops must always pass one."
  ([argv] (run argv nil))
  ([argv {:keys [timeout-ms]}]
   (let [process (.start (ProcessBuilder. ^java.util.List (vec argv)))
         out (atom "")
         err (atom "")
         stream-error (atom nil)
         _ (.close (.getOutputStream process))
         out-thread (Thread/startVirtualThread
                      ^Runnable
                      #(capture-stream! (.getInputStream process) out stream-error))
         err-thread (Thread/startVirtualThread
                      ^Runnable
                      #(capture-stream! (.getErrorStream process) err stream-error))
         exited? (if timeout-ms
                   (.waitFor process (long timeout-ms) TimeUnit/MILLISECONDS)
                   (do (.waitFor process) true))]
     (when-not exited?
       (kill-tree! process)
       (.join out-thread 1000)
       (.join err-thread 1000)
       (throw (ex-info (str "Command timed out after " timeout-ms " ms: "
                            (first argv))
                       {:type :subprocess-timeout
                        :command (first argv)
                        :timeout-ms timeout-ms})))
     (.join out-thread)
     (.join err-thread)
     (when-let [cause @stream-error]
       (throw cause))
     {:exit (.exitValue process) :out @out :err @err})))
