(ns test-support
  "Shared helpers for subprocess-based hook integration tests.

  Hooks were historically subprocessed via 'bb -cp src:resources -m hooks.X'
  but bb is no longer a project dependency. We invoke them through the JVM
  with a precomputed classpath so the tests don't depend on bb being on
  PATH. Classpath is computed once via 'clj -Spath' and reused."
  (:require [babashka.process :as p]
            [cch.db]
            [clojure.string :as str]))

(defn- project-root []
  (str/trim (:out (p/sh ["git" "rev-parse" "--show-toplevel"]))))

(defn- absolutize-cp
  "clj -Spath emits relative paths for :paths entries (e.g. 'src',
  'resources'). Tests subprocess hooks with :dir set to a temp git repo,
  which would break those relative entries. Rewrite each entry to an
  absolute path rooted at the project."
  [cp root]
  (->> (str/split cp #":")
       (map (fn [e]
              (if (or (str/starts-with? e "/") (str/blank? e))
                e
                (str root "/" e))))
       (str/join ":")))

(defonce ^{:doc "Project classpath, computed once with absolute paths."} project-cp
  (delay
    (let [root (project-root)]
      (absolutize-cp (str/trim (:out (p/sh {:dir root} "clj" "-Spath")))
                     root))))

(defn- db-path-prop
  "Propagate the test event-DB override into a subprocessed hook.

  A fresh JVM inherits no system properties, so without this the
  subprocess falls back to ~/.local/share/cch/events.db and writes
  fixtures into the real log — which is how 656 synthetic command-audit
  blocks got there.

  Skipped when the caller sets XDG_DATA_HOME itself: those tests point
  the hook at their own temp directory and then read back from it, and
  the property outranks the environment, so forcing it here would send
  the write somewhere the assertion is not looking. Absent outside a
  test run, where the default path is the correct one."
  [opts]
  (when-not (get-in opts [:extra-env "XDG_DATA_HOME"])
    (when-let [p (System/getProperty cch.db/path-property)]
      [(str "-D" cch.db/path-property "=" p)])))

(defn run-hook
  "Subprocess a hook by namespace name (e.g. \"hooks.event-log\") with the
  given JSON string on stdin. opts is a babashka.process options map (e.g.
  :dir, :extra-env). Returns the p/sh result map ({:exit :out :err})."
  [hook-ns json-input opts]
  (apply p/sh (merge {:in json-input} opts)
         (concat ["java"] (db-path-prop opts)
                 ["-cp" @project-cp "clojure.main" "-m" hook-ns])))
