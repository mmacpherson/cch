(ns cch.control.unix-websocket-test
  (:require [cch.control.unix-websocket :as websocket]
            [clojure.test :refer [deftest is]])
  (:import [java.io ByteArrayInputStream ByteArrayOutputStream]
           [java.nio.charset StandardCharsets]))

(deftest client-frame-round-trips-mask-and-all-supported-length-encodings
  (doseq [size [5 200 70000]]
    (let [text (apply str (take size (cycle "synthetic-frame-payload")))
          payload (.getBytes text StandardCharsets/UTF_8)
          output (ByteArrayOutputStream.)]
      (#'websocket/write-frame! output 0x1 payload)
      (let [frame (#'websocket/read-frame!
                    (ByteArrayInputStream. (.toByteArray output)))]
        (is (:final? frame))
        (is (= 0x1 (:opcode frame)))
        (is (= text (String. ^bytes (:payload frame)
                             StandardCharsets/UTF_8)))))))
