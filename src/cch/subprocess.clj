(ns cch.subprocess
  "Small synchronous subprocess boundary for short-lived CLI integrations.

  Output streams are drained concurrently on virtual threads. Unlike future-
  backed helpers, these joined threads do not keep one-shot cch JVMs alive for
  an executor keepalive period after the command has completed."
  (:import [java.nio.charset StandardCharsets]))

(defn- capture-stream! [stream destination error]
  (try
    (reset! destination (String. (.readAllBytes stream)
                                  StandardCharsets/UTF_8))
    (catch Exception cause
      (reset! error cause))))

(defn run
  "Run argv without a shell and return {:exit :out :err}."
  [argv]
  (let [process (.start (ProcessBuilder. ^java.util.List (vec argv)))
        out (atom "")
        err (atom "")
        stream-error (atom nil)
        out-thread (Thread/startVirtualThread
                     ^Runnable
                     #(capture-stream! (.getInputStream process) out stream-error))
        err-thread (Thread/startVirtualThread
                     ^Runnable
                     #(capture-stream! (.getErrorStream process) err stream-error))
        exit (.waitFor process)]
    (.join out-thread)
    (.join err-thread)
    (when-let [cause @stream-error]
      (throw cause))
    {:exit exit :out @out :err @err}))
