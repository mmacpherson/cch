(ns cch.control.naming-test
  (:require [cch.control.naming :as naming]
            [clojure.test :refer [deftest is]]))

(deftest mnemonics-are-stable-route-only-and-disambiguated
  (let [route "codex:00000000-0000-0000-0000-00000000000a"
        value (naming/mnemonic route)]
    (is (= value (naming/mnemonic route)))
    (is (re-matches #"[a-z]+-[a-z]+-[0-9a-f]{4}" value))
    (is (not= value
              (naming/mnemonic
                "codex:00000000-0000-0000-0000-00000000000b")))))

(deftest aliases-are-bounded-and-safe
  (is (= "Review pair" (naming/normalize-alias "  Review pair  ")))
  (is (nil? (naming/normalize-alias "   ")))
  (doseq [value [(apply str (repeat (inc naming/max-alias-length) "x"))
                 "forged\nname"
                 "right\u202eto-left"]]
    (is (= :invalid-alias
           (try (naming/normalize-alias value) nil
                (catch clojure.lang.ExceptionInfo error
                  (:type (ex-data error))))))))

(deftest explicit-alias-never-replaces-routing-identity
  (let [session (naming/present-session
                  {:id "claude:00000000-0000-0000-0000-00000000000c"
                   :agent "claude" :alias "Audit"})]
    (is (= "claude:00000000-0000-0000-0000-00000000000c" (:id session)))
    (is (= "Audit" (:alias session)))
    (is (= (str "Audit · " (:mnemonic session)) (:display-name session)))))
