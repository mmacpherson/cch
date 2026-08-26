(ns cch.subprocess-test
  (:require [cch.subprocess :as subprocess]
            [clojure.test :refer [deftest is]]))

(deftest captures-stdout-stderr-and-exit-without-a-shell
  (let [result (subprocess/run
                 ["bash" "-c" "printf out; printf err >&2; exit 7"])]
    (is (= 7 (:exit result)))
    (is (= "out" (:out result)))
    (is (= "err" (:err result)))))
