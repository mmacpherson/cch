(ns cch.summarizer-client
  "The one place cch calls the Anthropic API: a single structured-output
  request per summary, through the official Java SDK. The API key is read
  from a local file and never logged."
  (:require [babashka.fs :as fs]
            [cheshire.core :as json]
            [clojure.string :as str]
            [clojure.walk :as walk])
  (:import [com.anthropic.client AnthropicClient]
           [com.anthropic.client.okhttp AnthropicOkHttpClient]
           [com.anthropic.core JsonValue]
           [com.anthropic.models.messages JsonOutputFormat JsonOutputFormat$Schema
            Message MessageCreateParams OutputConfig OutputConfig$Effort]
           [java.time Duration]))

(def default-key-file
  (str (System/getProperty "user.home") "/.local/share/cch/anthropic-api-key"))

(defn read-key
  "The API key from key-file, or nil when the file is missing or empty."
  [key-file]
  (when (and key-file (fs/exists? key-file))
    (not-empty (str/trim (slurp key-file)))))

(defonce ^:private clients (atom {}))

(defn- client ^AnthropicClient [api-key]
  (or (get @clients api-key)
      (let [c (-> (AnthropicOkHttpClient/builder)
                  (.apiKey api-key)
                  (.timeout (Duration/ofSeconds 60))
                  (.maxRetries 2)
                  .build)]
        ;; Keep one client: a key rotation replaces the map entry.
        (reset! clients {api-key c})
        c)))

(defn- schema ^JsonOutputFormat$Schema [schema-map]
  (let [b (JsonOutputFormat$Schema/builder)]
    (doseq [[k v] (walk/stringify-keys schema-map)]
      (.putAdditionalProperty b k (JsonValue/from v)))
    (.build b)))

(defn- message-text [^Message m]
  (->> (.content m)
       (keep #(some-> (.text %) (.orElse nil) .text))
       (str/join "")))

(defn call!
  "Send one request; return {:text :input-tokens :output-tokens :stop-reason}.
  Throws on transport or API errors; callers decide whether to retry."
  [{:keys [api-key model system user schema-map max-tokens]
    :or {max-tokens 1024}}]
  (let [params (-> (MessageCreateParams/builder)
                   (.model ^String model)
                   (.maxTokens (long max-tokens))
                   (.system ^String system)
                   (.outputConfig (-> (OutputConfig/builder)
                                      (.effort OutputConfig$Effort/LOW)
                                      (.format (-> (JsonOutputFormat/builder)
                                                   (.schema (schema schema-map))
                                                   .build))
                                      .build))
                   (.addUserMessage ^String user)
                   .build)
        ^Message m (.create (.messages (client api-key)) params)
        usage (.usage m)]
    {:text (message-text m)
     :input-tokens (.inputTokens usage)
     :output-tokens (.outputTokens usage)
     :stop-reason (some-> (.stopReason m) (.orElse nil) str)}))

(defn json-text->map
  "Parse model JSON text, or nil."
  [text]
  (try (json/parse-string text true) (catch Exception _ nil)))
