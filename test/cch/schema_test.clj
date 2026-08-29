(ns cch.schema-test
  (:require [cch.schema :as schema]
            [clojure.string :as str]
            [clojure.test :refer [deftest is]]))

(deftest boundary-errors-do-not-echo-rejected-data
  (let [validator (schema/validator
                    [:map {:closed true} [:safe-field string?]])
        private-value "synthetic-sensitive-value"
        error (try
                (schema/validate! validator
                                  {:unexpected-field private-value}
                                  :invalid-synthetic-boundary
                                  "Synthetic boundary is invalid")
                (catch clojure.lang.ExceptionInfo exception exception))]
    (is (= :invalid-synthetic-boundary (:type (ex-data error))))
    (is (= {:type :invalid-synthetic-boundary} (ex-data error)))
    (is (not (str/includes? (str error) private-value)))))
