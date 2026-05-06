(ns build
  "Build pipeline: uberjar (clojure.tools.build) + jlink runtime image.

  Targets (invoked via `clj -T:build <fn>`):
    clean   — remove target/
    uber    — produce target/cch.jar (AOT-compiled, all deps bundled)
    runtime — produce target/cch-runtime/ — slim JRE + uberjar + launcher
    all     — clean + uber + runtime

  The runtime directory is self-contained: copy it to a machine with no
  JDK installed and `target/cch-runtime/bin/cch serve` works."
  (:require [clojure.tools.build.api :as b]
            [clojure.java.io :as io]
            [clojure.java.shell :as sh]
            [clojure.string :as str]))

(def ^:private class-dir "target/classes")
(def ^:private uber-file "target/cch.jar")
(def ^:private runtime-dir "target/cch-runtime")
(def ^:private main-ns 'cli.cch)
(def ^:private basis (delay (b/create-basis {:project "deps.edn"})))

(defn clean [_]
  (b/delete {:path "target"}))

(defn uber
  "Compile the project AOT and produce target/cch.jar. The Main-Class is
  cli.cch (gen-classed), so `java -jar target/cch.jar <args>` runs the
  CLI directly."
  [_]
  (b/delete {:path class-dir})
  (b/copy-dir {:src-dirs   ["src" "resources"]
               :target-dir class-dir})
  (b/compile-clj {:basis      @basis
                  :src-dirs   ["src"]
                  :class-dir  class-dir
                  ;; AOT only the entry point — its transitive requires
                  ;; (cch.server, hooks, etc.) are loaded at runtime from
                  ;; the source bundled into the jar by copy-dir.
                  :ns-compile [main-ns]})
  (b/uber {:class-dir class-dir
           :uber-file uber-file
           :basis     @basis
           :main      main-ns})
  (println "Built" uber-file
           (format "(%.1f MB)" (/ (.length (io/file uber-file)) 1024.0 1024.0))))

;; jdeps refuses to analyze our multi-release uberjar because it sees
;; jackson cbor's module ref to jackson core but those jars live inside
;; the uber, not on the module path. The set of JDK modules cch needs is
;; small and stable, so just enumerate them.
(def ^:private required-modules
  ["java.base"
   "java.logging"        ;; clojure, http-kit
   "java.management"     ;; JMX / ThreadMXBean (forecast diagnostics)
   "java.naming"         ;; InetAddress, used by nrepl + http-kit
   "java.net.http"       ;; http-client used by hook tests / cli
   "java.security.jgss"  ;; transitive from http-client TLS
   "java.sql"            ;; sqlite-jdbc
   "java.xml"            ;; clj-yaml/snakeyaml
   "jdk.crypto.ec"       ;; TLS EC keys
   "jdk.unsupported"     ;; sun.misc.Unsafe (clojure runtime)
   "jdk.zipfs"])         ;; jar/classpath FS provider

(defn runtime
  "Build a slim JRE under target/cch-runtime/ via jlink. The module set
  is hardcoded (jdeps can't analyze our multi-release uber) but small
  enough to audit by eye."
  [_]
  (when-not (.exists (io/file uber-file))
    (uber nil))
  (b/delete {:path runtime-dir})
  (let [modules (str/join "," required-modules)]
    (println "jlink modules:" modules)
    (let [{:keys [exit err]}
          (sh/sh "jlink"
                 "--add-modules" modules
                 "--strip-debug"
                 "--no-man-pages"
                 "--no-header-files"
                 "--compress" "zip-6"
                 "--output" runtime-dir)]
      (when-not (zero? exit)
        (throw (ex-info (str "jlink failed: " err) {:exit exit})))))
  ;; Copy the uberjar inside the runtime image so the whole bundle
  ;; relocates as a unit.
  (let [lib-dir (io/file runtime-dir "lib")]
    (.mkdirs lib-dir)
    (io/copy (io/file uber-file) (io/file lib-dir "cch.jar")))
  ;; Launcher script.
  (let [launcher (io/file runtime-dir "bin" "cch")]
    (spit launcher
          (str "#!/usr/bin/env bash\n"
               "# cch launcher — runs the bundled JRE against the bundled uberjar.\n"
               "#\n"
               "# JVM tuning: cch handles ~thousands of cheap dispatches/day. We don't\n"
               "# need a multi-GB heap or a parallel GC; SerialGC has the smallest\n"
               "# footprint and is appropriate for a low-throughput single-process\n"
               "# server. -Xmx256m caps total heap. We tried -Xmx128m and RSS went\n"
               "# UP (~231MB vs ~211MB) — tighter heap means more GC churn copying\n"
               "# survivors, and heap is only a slice of RSS anyway (metaspace,\n"
               "# code cache, mmapped jar, native libs make up the rest).\n"
               "# Override via $CCH_JAVA_OPTS for debugging.\n"
               "set -euo pipefail\n"
               "here=\"$(cd \"$(dirname \"$0\")/..\" && pwd)\"\n"
               "exec \"$here/bin/java\" \\\n"
               "  -Xmx256m \\\n"
               "  -XX:+UseSerialGC \\\n"
               "  -XX:MaxMetaspaceSize=128m \\\n"
               "  ${CCH_JAVA_OPTS:-} \\\n"
               "  -jar \"$here/lib/cch.jar\" \"$@\"\n"))
    (.setExecutable launcher true false))
  (let [{:keys [out]} (sh/sh "du" "-sh" runtime-dir)]
    (println "Built" runtime-dir (str/trim out))))

(defn all [_]
  (clean nil)
  (uber nil)
  (runtime nil))
