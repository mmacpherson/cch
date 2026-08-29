(ns cch.schema
  "Malli helpers for external trust boundaries.

  Error responses deliberately carry only a stable cch error type. Malli's
  explanations include rejected values, so they remain an internal debugging
  concern and must not be attached to exceptions crossing HTTP or MCP."
  (:require [malli.core :as m]))

(defn validator
  "Compile one schema form and return its reusable predicate. Define the result
  once at namespace load rather than compiling schemas on the request path."
  [schema-form]
  (m/validator (m/schema schema-form)))

(defn validate!
  "Return value when validator accepts it; otherwise throw a sanitized error.
  Neither the rejected value nor Malli's explanation enters ex-data."
  [validator value error-type message]
  (when-not (validator value)
    (throw (ex-info message {:type error-type})))
  value)
