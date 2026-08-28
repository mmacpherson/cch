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

(deftest native-names-are-bounded-and-safe
  (is (= "Review panel"
         (naming/normalize-native-name "  Review panel  ")))
  (is (nil? (naming/normalize-native-name "   ")))
  (doseq [value [(apply str (repeat (inc naming/max-native-name-length) "x"))
                 "forged\nname"
                 "right\u202eto-left"]]
    (is (= :invalid-native-name
           (try (naming/normalize-native-name value) nil
                (catch clojure.lang.ExceptionInfo error
                  (:type (ex-data error))))))))

(deftest explicit-alias-never-replaces-routing-identity
  (let [session (naming/present-session
                  {:id "claude:00000000-0000-0000-0000-00000000000c"
                   :agent "claude" :alias "Audit"})]
    (is (= "claude:00000000-0000-0000-0000-00000000000c" (:id session)))
    (is (= "Audit" (:alias session)))
    (is (= (str "Audit · " (:mnemonic session)) (:display-name session)))))

(deftest explicit-alias-precedes-native-name-which-precedes-fallback
  (let [route "claude:00000000-0000-0000-0000-00000000000d"
        native (naming/present-session {:id route :name "Panel name"})
        aliased (naming/present-session
                  {:id route :name "Panel name" :alias "Operator name"})]
    (is (= "Panel name" (:name native)))
    (is (= (str "Panel name · " (:mnemonic native))
           (:display-name native)))
    (is (= (str "Operator name · " (:mnemonic aliased))
           (:display-name aliased)))
    (is (= "Panel name" (:name aliased)))))

(deftest invalid-stored-alias-does-not-hide-a-valid-native-name
  (let [session (naming/present-session
                  {:id "codex:00000000-0000-0000-0000-00000000000e"
                   :name "Panel name" :alias "forged\nname"})]
    (is (nil? (:alias session)))
    (is (= (str "Panel name · " (:mnemonic session))
           (:display-name session)))))
