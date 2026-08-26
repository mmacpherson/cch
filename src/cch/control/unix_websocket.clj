(ns cch.control.unix-websocket
  "Minimal synchronous WebSocket client over a Unix domain socket.

  Codex's app-server Unix listener uses an HTTP Upgrade followed by one JSON-RPC
  message per text frame. The JDK WebSocket client cannot dial Unix sockets, so
  this namespace implements only the small RFC 6455 client surface cch needs."
  (:require [clojure.string :as str])
  (:import [java.io BufferedInputStream BufferedOutputStream
            ByteArrayOutputStream EOFException InputStream OutputStream]
           [java.net StandardProtocolFamily UnixDomainSocketAddress]
           [java.nio.channels Channels SocketChannel]
           [java.nio.charset StandardCharsets]
           [java.nio.file Path]
           [java.security MessageDigest SecureRandom]
           [java.util Base64]))

(def ^:private websocket-guid "258EAFA5-E914-47DA-95CA-C5AB0DC85B11")
(def ^:private max-header-bytes (* 16 1024))
(def ^:private max-message-bytes (* 16 1024 1024))
(def ^:private secure-random (SecureRandom.))

(defprotocol TextTransport
  (send-text! [transport text])
  (read-text! [transport])
  (close! [transport]))

(defn- read-byte! [^InputStream input]
  (let [value (.read input)]
    (when (= -1 value)
      (throw (EOFException. "WebSocket connection closed")))
    value))

(defn- read-exactly! [^InputStream input length]
  (let [bytes (byte-array length)]
    (loop [offset 0]
      (when (< offset length)
        (let [read-count (.read input bytes offset (- length offset))]
          (when (= -1 read-count)
            (throw (EOFException. "WebSocket connection closed mid-frame")))
          (recur (+ offset read-count)))))
    bytes))

(defn- read-http-headers! [^InputStream input]
  (let [buffer (ByteArrayOutputStream.)]
    (loop [tail []]
      (when (>= (.size buffer) max-header-bytes)
        (throw (ex-info "WebSocket upgrade response headers are too large"
                        {:type :websocket-upgrade-failed})))
      (let [value (read-byte! input)
            next-tail (->> (conj tail value) (take-last 4) vec)]
        (.write buffer value)
        (if (= [13 10 13 10] next-tail)
          (.toString buffer StandardCharsets/US_ASCII)
          (recur next-tail))))))

(defn- parse-http-response [response]
  (let [[status-line & header-lines] (str/split response #"\r\n")
        headers (into {}
                      (keep (fn [line]
                              (when-let [separator (str/index-of line ":")]
                                [(-> (subs line 0 separator) str/lower-case)
                                 (-> (subs line (inc separator)) str/trim)])))
                      header-lines)]
    {:status-line status-line :headers headers}))

(defn- expected-accept [key]
  (let [digest (doto (MessageDigest/getInstance "SHA-1")
                 (.update (.getBytes (str key websocket-guid)
                                     StandardCharsets/US_ASCII)))]
    (.encodeToString (Base64/getEncoder) (.digest digest))))

(defn- upgrade! [^InputStream input ^OutputStream output]
  (let [nonce (byte-array 16)
        _ (.nextBytes secure-random nonce)
        key (.encodeToString (Base64/getEncoder) nonce)
        request (str "GET / HTTP/1.1\r\n"
                     "Host: localhost\r\n"
                     "Upgrade: websocket\r\n"
                     "Connection: Upgrade\r\n"
                     "Sec-WebSocket-Key: " key "\r\n"
                     "Sec-WebSocket-Version: 13\r\n\r\n")]
    (.write output (.getBytes request StandardCharsets/US_ASCII))
    (.flush output)
    (let [{:keys [status-line headers]} (parse-http-response
                                          (read-http-headers! input))]
      (when-not (re-find #"^HTTP/1\.[01] 101(?: |$)" status-line)
        (throw (ex-info (str "WebSocket upgrade rejected: " status-line)
                        {:type :websocket-upgrade-failed
                         :status-line status-line})))
      (when-not (= (expected-accept key) (get headers "sec-websocket-accept"))
        (throw (ex-info "WebSocket upgrade returned an invalid accept key"
                        {:type :websocket-upgrade-failed}))))))

(defn- write-length! [^OutputStream output length masked?]
  (let [mask-bit (if masked? 0x80 0)]
    (cond
      (< length 126)
      (.write output (bit-or mask-bit length))

      (<= length 0xffff)
      (do
        (.write output (bit-or mask-bit 126))
        (.write output (bit-and 0xff (bit-shift-right length 8)))
        (.write output (bit-and 0xff length)))

      :else
      (do
        (.write output (bit-or mask-bit 127))
        (doseq [shift (range 56 -1 -8)]
          (.write output (bit-and 0xff (bit-shift-right length shift))))))))

(defn- write-frame! [^OutputStream output opcode ^bytes payload]
  (when (> (alength payload) max-message-bytes)
    (throw (ex-info "WebSocket message exceeds cch's size limit"
                    {:type :websocket-message-too-large
                     :size (alength payload)})))
  (let [mask (byte-array 4)
        masked (byte-array (alength payload))]
    (.nextBytes secure-random mask)
    (dotimes [index (alength payload)]
      (aset-byte masked index
                 (unchecked-byte
                   (bit-xor (bit-and 0xff (aget payload index))
                            (bit-and 0xff (aget mask (mod index 4)))))))
    (.write output (bit-or 0x80 opcode))
    (write-length! output (alength payload) true)
    (.write output mask)
    (.write output masked)
    (.flush output)))

(defn- read-unsigned-long! [^InputStream input bytes]
  (loop [remaining bytes
         value 0]
    (if (zero? remaining)
      value
      (recur (dec remaining)
             (+ (bit-shift-left value 8) (read-byte! input))))))

(defn- read-frame! [^InputStream input]
  (let [first-byte (read-byte! input)
        second-byte (read-byte! input)
        final? (pos? (bit-and first-byte 0x80))
        opcode (bit-and first-byte 0x0f)
        masked? (pos? (bit-and second-byte 0x80))
        short-length (bit-and second-byte 0x7f)
        length (case short-length
                 126 (read-unsigned-long! input 2)
                 127 (read-unsigned-long! input 8)
                 short-length)]
    (when (> length max-message-bytes)
      (throw (ex-info "WebSocket frame exceeds cch's size limit"
                      {:type :websocket-message-too-large :size length})))
    (let [mask (when masked? (read-exactly! input 4))
          payload (read-exactly! input (int length))]
      (when mask
        (dotimes [index (alength payload)]
          (aset-byte payload index
                     (unchecked-byte
                       (bit-xor (bit-and 0xff (aget payload index))
                                (bit-and 0xff (aget mask (mod index 4))))))))
      {:final? final? :opcode opcode :payload payload})))

(defrecord UnixWebSocket [^SocketChannel channel
                          ^InputStream input
                          ^OutputStream output]
  TextTransport
  (send-text! [_ text]
    (write-frame! output 0x1 (.getBytes (str text) StandardCharsets/UTF_8)))
  (read-text! [_]
    (let [message (ByteArrayOutputStream.)]
      (loop [continuing? false]
        (let [{:keys [final? opcode payload]} (read-frame! input)]
          (case opcode
            0x0
            (if continuing?
              (do
                (.write message payload)
                (if final?
                  (.toString message StandardCharsets/UTF_8)
                  (recur true)))
              (throw (ex-info "Unexpected WebSocket continuation frame"
                              {:type :websocket-protocol-error})))

            0x1
            (if continuing?
              (throw (ex-info "New WebSocket text frame during continuation"
                              {:type :websocket-protocol-error}))
              (do
                (.write message payload)
                (if final?
                  (.toString message StandardCharsets/UTF_8)
                  (recur true))))

            0x8
            (throw (EOFException. "WebSocket peer closed the connection"))

            0x9
            (do (write-frame! output 0xA payload)
                (recur continuing?))

            0xA
            (recur continuing?)

            (throw (ex-info (str "Unsupported WebSocket opcode: " opcode)
                            {:type :websocket-protocol-error
                             :opcode opcode})))))))
  (close! [_]
    (try
      (write-frame! output 0x8 (byte-array 0))
      (catch Exception _ nil))
    (.close channel)))

(defn connect!
  "Connect and complete the WebSocket HTTP upgrade on socket-path."
  [socket-path]
  (let [channel (SocketChannel/open StandardProtocolFamily/UNIX)]
    (try
      (.connect channel (UnixDomainSocketAddress/of (Path/of socket-path
                                                              (make-array String 0))))
      (let [input (BufferedInputStream. (Channels/newInputStream channel))
            output (BufferedOutputStream. (Channels/newOutputStream channel))]
        (upgrade! input output)
        (->UnixWebSocket channel input output))
      (catch Exception error
        (.close channel)
        (throw error)))))
