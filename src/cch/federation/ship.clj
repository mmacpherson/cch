(ns cch.federation.ship
  "Cross-machine event-log federation — background shipper (n55).

  The networked half of federation: reads un-shipped local rows and POSTs
  them to the collector's /ingest endpoint, advancing a per-table watermark
  after each accepted batch. Runs on a daemon thread off the hook hot path,
  so a slow or unreachable collector never touches local write latency.

  Split from cch.federation so that cch.log (which stamps node on every
  write via cch.federation/node-name) does not transitively load hato /
  cheshire."
  (:require [cch.federation :as fed]
            [cheshire.core :as json]
            [org.httpkit.client :as http]))

(defn- post-batch!
  "POST one table batch to the collector. Returns true on HTTP 200.
  Uses http-kit's client (already a runtime dep); deref makes the async
  request synchronous for this background thread."
  [{:keys [collector-url token]} table rows]
  (try
    (let [resp @(http/request
                  {:url     (str collector-url "/ingest")
                   :method  :post
                   :body    (json/generate-string {:table table :rows rows})
                   :headers (cond-> {"Content-Type" "application/json"}
                              token (assoc "Authorization" (str "Bearer " token)))
                   :timeout 10000})]
      (= 200 (:status resp)))
    (catch Exception _ false)))

(def ^:private max-post-bytes
  "Cap on one POST body. Event `extra` blobs vary from bytes to tens of KB,
  so a fixed row count doesn't bound the payload — a 500-row batch can blow
  past http-kit's default 8MB :max-body and get dropped (broken pipe). Split
  by cumulative serialized size instead, keeping each POST small enough for
  the collector (and its modest heap) to build the INSERT."
  (* 4 1024 1024))

(defn into-byte-batches
  "Partition `rows` (id-ascending) into contiguous groups whose JSON stays
  under max-post-bytes. A single oversized row forms its own group so the
  drain can't wedge on it."
  [rows]
  (let [{:keys [batches cur]}
        (reduce (fn [{:keys [batches cur cur-bytes]} r]
                  (let [rb (count (json/generate-string r))]
                    (if (and (seq cur) (> (+ cur-bytes rb) max-post-bytes))
                      {:batches (conj batches cur) :cur [r] :cur-bytes rb}
                      {:batches batches :cur (conj cur r) :cur-bytes (+ cur-bytes rb)})))
                {:batches [] :cur [] :cur-bytes 0}
                rows)]
    (cond-> batches (seq cur) (conj cur))))

(defn ship-table-once!
  "Ship all un-shipped rows of `table`, draining in id order and advancing
  the watermark after each accepted POST. Rows are byte-batched so no single
  POST exceeds the collector's body limit. Stops on the first failed POST
  (the watermark isn't advanced past it, so it retries next cycle). Returns
  the number of rows shipped."
  [cfg table]
  (loop [shipped 0]
    (let [wm   (fed/get-watermark table)
          raw  (fed/rows-after table wm fed/ship-batch)
          ;; Guarantee a non-null node on shipped rows so the collector's
          ;; UNIQUE(node, origin_id) dedup is well-defined even for rows
          ;; written before node stamping existed.
          rows (mapv #(assoc % :node (or (:node %) (:node cfg))) raw)]
      (if (empty? rows)
        shipped
        (let [[sent stopped?]
              (reduce (fn [[sent _] chunk]
                        (if (post-batch! cfg table chunk)
                          (do (fed/set-watermark! table (apply max (map :id chunk)))
                              [(+ sent (count chunk)) false])
                          (reduced [sent true])))
                      [0 false]
                      (into-byte-batches rows))]
          (if (or stopped? (< (count rows) fed/ship-batch))
            (+ shipped sent)
            (recur (+ shipped sent))))))))

(defn ship-once!
  "One full ship cycle across every shippable table. Returns a
  {table → rows-shipped} map."
  [cfg]
  (reduce (fn [acc t] (assoc acc t (ship-table-once! cfg t)))
          {}
          ["events" "context_snapshots"]))

(defonce ^:private shipper (atom nil))

(defn start-shipper!
  "Start the background shipper if federation shipping is enabled in the
  global config. Idempotent. Returns :started, :disabled, or :already."
  []
  (let [cfg (fed/load-federation-config)]
    (cond
      (not (:enabled? cfg)) :disabled
      @shipper              :already
      :else
      (let [stop   (atom false)
            thread (Thread.
                     ^Runnable
                     (fn []
                       (try
                         (while (not @stop)
                           (try (ship-once! cfg) (catch Exception _ nil))
                           (Thread/sleep (:interval-ms cfg)))
                         (catch InterruptedException _ nil))))]
        (.setDaemon thread true)
        (.setName thread "cch-federation-shipper")
        (.start thread)
        (reset! shipper {:stop stop :thread thread})
        :started))))

(defn stop-shipper!
  "Signal the shipper thread to stop and drop it. Idempotent."
  []
  (when-let [{:keys [stop ^Thread thread]} @shipper]
    (reset! stop true)
    (.interrupt thread)
    (reset! shipper nil)))
