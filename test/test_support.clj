(ns test-support
  "Shared helpers for subprocess-based hook integration tests.

  Hooks were historically subprocessed via 'bb -cp src:resources -m hooks.X'
  but bb is no longer a project dependency. We invoke them through the JVM
  with a precomputed classpath so the tests don't depend on bb being on
  PATH. Classpath is computed once via 'clj -Spath' and reused."
  (:require [babashka.process :as p]
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

(defn run-hook
  "Subprocess a hook by namespace name (e.g. \"hooks.event-log\") with the
  given JSON string on stdin. opts is a babashka.process options map (e.g.
  :dir, :extra-env). Returns the p/sh result map ({:exit :out :err})."
  [hook-ns json-input opts]
  (p/sh (merge {:in json-input} opts)
        "java" "-cp" @project-cp "clojure.main" "-m" hook-ns))
