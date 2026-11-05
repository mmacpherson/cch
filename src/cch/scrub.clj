(ns cch.scrub
  "Pure, best-effort privacy boundary for outgoing digests and incoming summaries.
  Counts contain kinds and totals only. Call on both sides of the model boundary;
  this heuristic scrubber is defense in depth, not a secrecy guarantee."
  (:require [clojure.string :as str]))

(def default-max-chars 12000)

(defn- entropy [s]
  (let [n (double (count s))]
    (- (reduce + (map (fn [c] (let [p (/ c n)] (* p (/ (Math/log p) (Math/log 2)))))
                      (vals (frequencies s)))))))

(def ^:private rules
  ;; Order matters: remove enclosing credentials before inspecting their pieces.
  [[:pem #"(?s)-----BEGIN [A-Z0-9 ]*(?:PRIVATE KEY|CERTIFICATE)-----.*?-----END [A-Z0-9 ]*(?:PRIVATE KEY|CERTIFICATE)-----"]
   [:auth #"(?i)\b(?:bearer|basic)\s+[A-Za-z0-9+/_.=~:-]+"]
   [:credential #"(?i)((?<![a-z0-9_-])(?:--)?(?:[a-z0-9]+[_-])*(?:token|secret|password|passwd|api[_-]?key|access[_-]?key|auth)(?:[_-][a-z0-9]+)*[\"']?\s*[:=]\s*|--(?:password|passwd|token|secret|api-key|apikey|access-key|auth-token)\s+)(?:\"[^\"]*\"|'[^']*'|[^\s,;\}\]\"']+)"]
   [:token #"\b(?:sk-[A-Za-z0-9_-]{8,}|gh[pousr]_[A-Za-z0-9_]{8,}|github_pat_[A-Za-z0-9_]{8,}|xox[baprs]-[A-Za-z0-9_-]{8,}|AKIA[A-Z0-9]{16}|glpat-[A-Za-z0-9_-]{8,}|AIza[A-Za-z0-9_-]{8,}|[srp]k_live_[A-Za-z0-9_-]{8,}|npm_[A-Za-z0-9_-]{8,}|hf_[A-Za-z0-9_-]{8,}|xapp-[A-Za-z0-9_-]{8,}|ya29\.[A-Za-z0-9_-]{8,})(?![A-Za-z0-9_-])"]
   [:jwt #"\beyJ[A-Za-z0-9_-]+\.[A-Za-z0-9_-]+\.[A-Za-z0-9_-]+\b"]
   [:userinfo #"(?i)[a-z][a-z0-9+.-]*://[^\s/@]+(?::[^\s/@]*)?@"]
   [:email #"(?i)[a-z0-9.!#$%&'*+/=?^_`{|}~-]+@[a-z0-9-]+(?:\.[a-z0-9-]+)+"]
   [:home #"(?:/home/|/Users/)[^/\s\"'<>]+"]
   [:ip #"(?<![\w.])(?:\d{1,3}\.){3}\d{1,3}(?![\w.])"]
   [:ip #"(?i)(?<![\w:])(?=[a-f0-9:]*::)[a-f0-9:]*::[a-f0-9:]*(?![\w:])"]
   [:ip #"(?i)(?<![\w:])(?:[a-f0-9]{1,4}:){7}[a-f0-9]{1,4}(?![\w:])"]
   [:phone #"(?<!\w)\+[1-9]\d{7,14}(?!\w)"]
   [:phone #"(?<!\w)(?:\+\d{1,3}[ .-]?)?(?:\(\d{3}\)|\d{3})[ .-]\d{3}[ .-]\d{4}(?!\w)"]
   ;; Dotted words in prose are commonly source files or namespaces.
   ;; Recognize hosts only in URL authority positions; known-hosts handles
   ;; private single-word names and host references in ordinary prose.
   [:hostname #"(?i)(?<![\w./-])[a-z0-9-]+(?:\.[a-z0-9-]+)*\.(?:local|lan|internal|home\.arpa|ts\.net|corp|invalid)(?![\w-]|\.[a-z0-9])"]
   [:hostname #"(?i)([a-z][a-z0-9+.-]*://)([a-z0-9-]+(?:\.[a-z0-9-]+)+)"]])


(defn scrub
  "Return {:text string :counts {kind n}} without I/O or value-bearing logs.
  Options: :max-chars (default 12000), :known-hosts (map of exact machine names to safe display labels; nil labels
  use typed placeholders),
  :extra-patterns [{:kind keyword :pattern regex-string-or-Pattern} ...].
  Invalid configuration throws before returning any text. Extra patterns replace
  whole matches. Home prefixes become ~; other matches use typed placeholders."
  ([text] (scrub text {}))
  ([text {:keys [max-chars known-hosts extra-patterns]
          :or {max-chars default-max-chars}}]
   (when-not (and (integer? max-chars) (<= 0 max-chars))
     (throw (ex-info "max-chars must be a nonnegative integer" {})))
   (let [extras (mapv (fn [{:keys [kind pattern]}]
                        (when-not (and (keyword? kind) (or (string? pattern) (instance? java.util.regex.Pattern pattern)))
                          (throw (ex-info "Invalid extra scrub pattern" {})))
                        [kind (re-pattern pattern)]) extra-patterns)
         counts (volatile! {})
         replacement (fn [kind]
                       (vswap! counts update kind (fnil inc 0))
                       (if (= :home kind) "~" (str "[redacted:" (name kind) "]")))
         cleaned (reduce (fn [s [kind pattern]]
                           (str/replace s pattern
                             (fn [match]
                               (let [placeholder (replacement kind)]
                                 (if (and (vector? match) (#{:credential :hostname} kind))
                                   (str (second match) placeholder)
                                   placeholder)))))
                         (reduce (fn [s [host label]]
                                   (str/replace s
                                     (re-pattern (str "(?i)(?<![\\w.-])" (java.util.regex.Pattern/quote host) "(?![\\w.-])"))
                                     (fn [_]
                                       (let [placeholder (replacement :hostname)]
                                         (if label (:text (scrub label)) placeholder)))))
                                 (str text) known-hosts)
                         (concat extras rules))
         cleaned (str/replace cleaned #"[A-Za-z0-9_+/=-]{24,}"
                              (fn [s] (if (>= (entropy s) 4.0)
                                        (replacement :entropy) s)))
         capped (subs cleaned 0 (min (count cleaned) max-chars))
         ;; Do not publish partial placeholders at the length boundary.
         open (.lastIndexOf capped "[")
         capped (if (and (<= 0 open)
                         (not (str/includes? (subs capped open) "]"))
                         (str/starts-with? "[redacted:" (subs capped open)))
                  (subs capped 0 open)
                  (if (re-find #"\[redacted:[^\]]*$" capped)
                    (str/replace capped #"\[redacted:[^\]]*$" "")
                    capped))]
     {:text capped :counts @counts})))

(defn digest
  "Build and scrub allowlisted turn data. Accepts :prompt, :tool-summaries
  (strings, first line only), and :closing-message. All other fields are ignored.
  Callers must provide summaries, never raw tool input/output or file content."
  ([turn] (digest turn {}))
  ([{:keys [prompt tool-summaries closing-message]} opts]
   (scrub (str/join "\n" (filter string?
                                 (concat [prompt]
                                         (map #(when (string? %) (first (str/split-lines %))) tool-summaries)
                                         [closing-message]))) opts)))
