(ns cli.codex-settings
  "Sentinel-block read/modify/write of Codex's config.toml.

  The file lives at `$CODEX_HOME/config.toml`, falling back to
  `~/.codex/config.toml` — see `codex-home`.

  Codex config is TOML and users edit it by hand, so we cannot round-trip
  through a Clojure TOML library (none preserve comments and key order).
  Instead, each cch-installed hook is wrapped in sentinel comment lines:

      # cch:begin <name>
      [[hooks.PreToolUse]]
      matcher = \".*\"
      [[hooks.PreToolUse.hooks]]
      type = \"command\"
      command = \"curl ...\"
      # cch:end <name>

  Install/uninstall operate purely as text splices between the sentinels.
  Everything outside any cch block is byte-preserved.

  See `claude-code-hooks-x9h` and 14s notes for the Codex TOML schema."
  (:require [babashka.fs :as fs]
            [cheshire.core :as json]
            [clojure.string :as str]))

(defn resolve-codex-home
  "Pick the Codex config root from a `CODEX_HOME` value and a home dir.
  Blank or missing env falls back to the upstream default. Pure."
  [codex-home-env home]
  (if (str/blank? codex-home-env)
    (str home "/.codex")
    codex-home-env))

(defn codex-home
  "Root of the user's Codex configuration.

  Codex honours `CODEX_HOME`, and users relocate it — an XDG override to
  `~/.config/codex` is common. Resolving it here rather than at call sites
  keeps every read and write on the file Codex actually loads; writing to a
  stale `~/.codex` fails silently, reporting success while installing
  nothing."
  []
  (resolve-codex-home (System/getenv "CODEX_HOME")
                      (System/getProperty "user.home")))

(defn codex-config-path
  "Path to the user's Codex config.toml."
  []
  (str (codex-home) "/config.toml"))

(defn read-config
  "Read config file as a string. Returns \"\" if missing."
  [path]
  (if (fs/exists? path)
    (slurp path)
    ""))

(defn write-config!
  "Write contents atomically (write to tmp, rename)."
  [path contents]
  (let [dir (or (fs/parent path) (fs/path "."))]
    (when-not (fs/exists? dir)
      (fs/create-dirs dir))
    (let [tmp (fs/create-temp-file {:dir dir :prefix ".cch-config-" :suffix ".toml"})]
      (try
        (try (fs/set-posix-file-permissions tmp "rw-------") (catch UnsupportedOperationException _ nil))
        (spit (str tmp) contents)
        (fs/move tmp path {:replace-existing true})
        (finally (fs/delete-if-exists tmp))))))

(defn- escape-toml-string
  "Minimal escape for a TOML basic string. Backslash and quote only —
  command strings we generate don't contain control chars."
  [s]
  (-> s
      (str/replace "\\" "\\\\")
      (str/replace "\"" "\\\"")))

(defn render-block
  "Render the TOML text for one cch-managed hook.

  `name` is the cch hook name (used in sentinels and as a stable id).
  `entries` is a seq of maps: {:event \"PreToolUse\" :matcher \".*\"
                                :command \"...\" :timeout 30}.
  All entries are emitted under one shared sentinel pair."
  [name entries]
  (let [body (->> entries
                  (map (fn [{:keys [event matcher command timeout]}]
                         (let [lines [(str "[[hooks." event "]]")
                                      (str "matcher = \"" (escape-toml-string matcher) "\"")
                                      (str "[[hooks." event ".hooks]]")
                                      "type = \"command\""
                                      (str "command = \"" (escape-toml-string command) "\"")]
                               lines (cond-> lines
                                       timeout (conj (str "timeout = " timeout)))]
                           (str/join "\n" lines))))
                  (str/join "\n\n"))]
    (str "# cch:begin " name "\n"
         body "\n"
         "# cch:end " name "\n")))

(def control-mcp-tools
  "Exact raw MCP tool names exposed to Codex. This is deliberately not a
  wildcard so adding a future cch tool cannot grant it ambient authority."
  ["list_sessions" "get_session" "send_message" "set_session_alias"])

(defn render-control-mcp-block
  "Render the complete cch-owned Codex MCP server configuration. Approval is
  scoped to this server, not Codex's global approval policy or auto mode."
  [{:keys [command args env]}]
  (let [quoted (fn [value]
                 (str "\"" (escape-toml-string value) "\""))
        array-value (fn [values]
                      (str "[" (str/join ", " (map quoted values)) "]"))
        env-lines (->> env
                       (sort-by key)
                       (map (fn [[key value]]
                              (str key " = " (quoted value)))))]
    (str "# cch:begin cch-control-mcp\n"
         "[mcp_servers.cch]\n"
         "command = " (quoted command) "\n"
         "args = " (array-value args) "\n"
         "enabled_tools = " (array-value control-mcp-tools) "\n"
         "default_tools_approval_mode = \"approve\"\n"
         "required = true\n"
         "\n[mcp_servers.cch.env]\n"
         (str/join "\n" env-lines) "\n"
         "# cch:end cch-control-mcp\n")))

(def control-mcp-token-env-var
  "Env var Codex reads the bearer token from (its config stores only the name,
  not the token). The value must be present in the Codex app-server's
  environment."
  "CCH_MCP_TOKEN")

(defn render-control-mcp-http-block
  "Render the cch-owned Codex MCP server as a streamable-HTTP server pointing
  at the shared `cch serve` endpoint. Codex reads the bearer token from the
  env var named here, so no token value is written into config.toml. Approval
  and the tool allowlist stay scoped to this server."
  [{:keys [url]}]
  (let [quoted (fn [value]
                 (str "\"" (escape-toml-string value) "\""))
        array-value (fn [values]
                      (str "[" (str/join ", " (map quoted values)) "]"))]
    (str "# cch:begin cch-control-mcp\n"
         "[mcp_servers.cch]\n"
         "url = " (quoted url) "\n"
         "bearer_token_env_var = " (quoted control-mcp-token-env-var) "\n"
         "enabled_tools = " (array-value control-mcp-tools) "\n"
         "default_tools_approval_mode = \"approve\"\n"
         "required = true\n"
         "# cch:end cch-control-mcp\n")))

(defn- block-pattern
  "Regex matching one named cch block. Consumes one leading newline (the
  separator inserted by `upsert-block`) and the block's own trailing
  newline (or EOF) — but never the user's file-terminating newline."
  [name]
  (let [q (java.util.regex.Pattern/quote name)]
    (re-pattern (str "(?m)(?:\\A|\\n)# cch:begin " q "\\n[\\s\\S]*?# cch:end " q "(?:\\n|\\z)"))))

(def ^:private any-block-pattern
  ;; Named-capture + back-reference forces begin/end names to match, so a
  ;; malformed file with mismatched sentinels won't have unrelated content
  ;; collapsed into one giant block.
  #"(?m)(?:\A|\n)# cch:begin (?<cchname>[^\n]+)\n[\s\S]*?# cch:end \k<cchname>(?:\n|\z)")

(declare ^:private block-spans ^:private remove-spans)

(defn strip-block
  "Remove the named cch block from `contents`. No-op if absent."
  [contents name]
  (remove-spans contents (block-spans contents (block-pattern name))))

(defn strip-all-blocks
  "Remove every cch sentinel block from `contents`."
  [contents]
  (remove-spans contents (block-spans contents any-block-pattern)))

;; Read string assignments without round-tripping TOML. Matching the whole
;; value also avoids interpreting commands or markers inside multiline strings.
(def ^:private string-field-pattern
  #"(?m)#[^\r\n]*|(?:^|[\s,{])([A-Za-z0-9_-]+|\"[^\"\r\n]+\"|'[^'\r\n]+')[ \t]*=[ \t]*(?:\"\"\"([\s\S]*?)\"\"\"|'''([\s\S]*?)'''|\"((?:\\.|[^\"\\\r\n])*)\"|'([^'\r\n]*)')")

(defn- string-fields [contents]
  (for [[_ key multi-basic multi-literal basic literal] (re-seq string-field-pattern (or contents ""))
        :when key]
    {:key (str/replace key #"^[\"']|[\"']$" "")
     :value (or multi-basic multi-literal
                (when basic
                  (try (json/parse-string (str "\"" basic "\""))
                       (catch Exception _ basic)))
                literal)}))

(defn- structure-text [contents]
  (str/replace (or contents "") string-field-pattern
               #(if (second %) (str/replace (first %) #"[^\r\n]" " ") (first %))))

(defn- block-spans [contents pattern]
  (let [m (re-matcher pattern (structure-text contents))]
    (loop [result []]
      (if (.find m)
        (recur (conj result {:start (.start m) :end (.end m)
                             :text (subs contents (.start m) (.end m))}))
        result))))

(defn- remove-spans [contents spans]
  (reduce (fn [s {:keys [start end]}] (str (subs s 0 start) (subs s end)))
          contents (reverse spans)))

(defn- dispatch-event [command]
  (when (and (re-find #"(?i)\bcurl\b" command)
             (re-find #"(?i)(?:(?:-X|--request)(?:=|\s*)[\"']?POST\b|--data(?:-binary|-raw|-ascii|-urlencode)?(?:=|\s))" command))
    (second (re-find #"/dispatch/([A-Za-z][A-Za-z0-9]*)" command))))

(defn dispatcher-diagnostics
  "Content-based dispatcher counts and malformed cch markers. No command
  values are returned. Best-effort TOML scanning; existing dispatch commands
  are preserved rather than rewritten, including hand-managed entries."
  [contents]
  (let [events (frequencies (keep #(when (= "command" (:key %)) (dispatch-event (:value %)))
                                  (string-fields contents)))
        outside-strings (structure-text contents)
        markers (keep-indexed (fn [index ln]
                                (when-let [[_ direction name] (re-matches #"\s*# cch:(begin|end) ([^\s]+)\s*" ln)]
                                  {:direction direction :name name :line (inc index)}))
                              (str/split-lines outside-strings))
        {:keys [stack issues]} (reduce (fn [{:keys [stack] :as state} marker]
                                         (if (= "begin" (:direction marker))
                                           (update state :stack conj marker)
                                           (if (= (:name marker) (:name (peek stack)))
                                             (update state :stack pop)
                                             (update state :issues conj marker))))
                                       {:stack [] :issues []} markers)]
    {:events events
     :duplicates (into {} (filter (fn [[_ n]] (> n 1))) events)
     :orphan-markers (vec (concat issues stack))
     :dispatcher-markers? (boolean (some #(= "cch-dispatcher" (:name %)) markers))}))

(defn dispatcher-present?
  "True when dispatcher content or its markers need preservation/review."
  [contents]
  (let [{:keys [events dispatcher-markers?]} (dispatcher-diagnostics contents)]
    (boolean (or (seq events) dispatcher-markers?))))

(defn- safe-hook-block?
  "A matched pair can have moved around unrelated TOML. Only splice it when
  its entire body consists of dispatcher hook tables and generated fields."
  [block command?]
  (let [fields (string-fields block)
        structure (structure-text block)]
    (and (every? #(contains? #{"matcher" "type" "command"} (:key %)) fields)
         (every? #(command? (:value %)) (filter #(= "command" (:key %)) fields))
         (= 2 (count (re-seq #"(?m)^\s*# cch:(?:begin|end) " structure)))
         (every? #(or (str/blank? %) (re-matches #"\s*#.*" %)
                      (re-matches #"\s*\[\[hooks\.[A-Za-z]+(?:\.hooks)?\]\]\s*" %)
                      (re-matches #"\s*timeout\s*=\s*\d+\s*(?:#.*)?" %))
                 (str/split-lines structure)))))

(defn upsert-block
  "Return new contents with the named cch block replaced (or appended).
  Structurally intact dispatcher blocks are refreshed. External dispatch
  commands or displaced markers cause preservation instead of duplication.
  Other blocks are replaced as before.
  Always separates new blocks from prior content by a blank line."
  [contents name entries]
  (let [blocks (map :text (block-spans contents (block-pattern name)))
        safe? (every? #(safe-hook-block? % dispatch-event) blocks)
        stripped (if (or (not= name "cch-dispatcher") safe?)
                   (strip-block contents name) contents)]
    (if (and (= name "cch-dispatcher") (dispatcher-present? stripped))
      contents
      (let [sep (cond
              (str/blank? stripped) ""
              (str/ends-with? stripped "\n\n") ""
              (str/ends-with? stripped "\n") "\n"
              :else "\n\n")]
        (str stripped sep (render-block name entries))))))

(defn install-hook!
  "Atomically install (or replace) the cch block for `name` in the
  Codex config at `path`. `entries` as in `render-block`."
  [path name entries]
  (let [contents (read-config path)
        updated (upsert-block contents name entries)]
    (when-not (= contents updated) (write-config! path updated))
    updated))

(defn- install-mcp-block!
  "Atomically replace the cch-owned MCP server block, preserving unrelated
  TOML. `render` produces the full sentinel-wrapped block from `config`."
  [path config render]
  (let [contents (read-config path)
        stripped (strip-block contents "cch-control-mcp")
        sep (cond
              (str/blank? stripped) ""
              (str/ends-with? stripped "\n\n") ""
              (str/ends-with? stripped "\n") "\n"
              :else "\n\n")
        updated (str stripped sep (render config))]
    (write-config! path updated)
    updated))

(defn install-control-mcp!
  "Atomically install the complete, allowlisted cch MCP server block (stdio
  transport — the fallback). Any prior cch-owned version is replaced while
  unrelated TOML is preserved."
  [path config]
  (install-mcp-block! path config render-control-mcp-block))

(defn install-control-mcp-http!
  "Atomically install the cch MCP server block as a streamable-HTTP server
  pointing at the shared endpoint. Replaces any prior cch-owned block."
  [path config]
  (install-mcp-block! path config render-control-mcp-http-block))

(defn remove-hook!
  "Atomically remove a single cch block by name."
  [path name]
  (let [updated (strip-block (read-config path) name)]
    (write-config! path updated)
    updated))

(defn remove-all-cch!
  "Atomically remove every cch sentinel block from the Codex config."
  [path]
  (let [updated (strip-all-blocks (read-config path))]
    (write-config! path updated)
    updated))

(defn- tables [contents]
  (let [structure (structure-text contents)
        matcher (re-matcher #"(?m)^[ \t]*(\[\[?[^\r\n]+?\]\]?)[ \t]*(?:#[^\r\n]*)?(?:\r?\n|$)" structure)
        headers (loop [result []]
                  (if (.find matcher)
                    (let [header (.group matcher 1)
                          inner (str/replace header #"^\[+|\]+$" "")
                          path (mapv #(str/replace % #"^[\"']|[\"']$" "")
                                     (re-seq #"\"[^\"\r\n]+\"|'[^'\r\n]+'|[A-Za-z0-9_-]+" inner))]
                      (recur (conj result {:start (.start matcher) :path path
                                           :array? (str/starts-with? header "[[")})))
                    result))]
    (mapv (fn [header end] (assoc header :end end :text (subs contents (:start header) end)))
          headers (concat (map :start (rest headers)) [(count contents)]))))

(defn control-mcp-info
  "Transport and ownership of the cch MCP table, never credential values."
  [contents]
  (let [ts (tables (or contents ""))
        main (first (filter #(and (not (:array? %)) (= ["mcp_servers" "cch"] (:path %))) ts))
        fields (into {} (map (juxt :key :value)) (string-fields (:text main)))
        command (get fields "command")
        url (get fields "url")
        args? (boolean (re-find #"(?s)\bargs\s*=\s*\[\s*[\"']control[\"']\s*,\s*[\"']mcp[\"']\s*\]"
                                (or (:text main) "")))
        tagged? (boolean (re-find #"(?m)^\s*# cch:begin cch-control-mcp\s*$" (structure-text contents)))
        owned? (or tagged? (and main (or (= "CCH_MCP_TOKEN" (get fields "bearer_token_env_var"))
                             (and command args? (re-find #"(?i)(?:^|[/\\])cch(?:\.exe)?$" command)))))]
    {:present? (boolean main) :owned? (boolean owned?)
     :transport (cond url :http command :stdio main :unknown :else :absent)}))

(defn strip-control-config
  "Remove cch-owned MCP tables and the intact source-binding hook. Table
  boundaries protect unrelated TOML even when cch comments were displaced.
  Unknown same-named servers and malformed binding blocks are preserved."
  [contents]
  (let [info (control-mcp-info contents)
        owned-tables (when (:owned? info)
                       (filter #(and (not (:array? %))
                                     (= ["mcp_servers" "cch"] (subvec (:path %) 0 (min 2 (count (:path %))))))
                               (tables contents)))
        stripped (reduce (fn [s {:keys [start text]}]
                           ;; Preserve comments/spacing after the final property.
                           (let [m (re-matcher #"(?s)(?:\r?\n[ \t]*(?:#[^\r\n]*)?)*$" text)
                                 _ (.find m)
                                 cut (+ start (.start m))]
                             (str (subs s 0 start) (subs s cut))))
                         contents (reverse owned-tables))
        stripped (if (:owned? info)
                   (remove-spans stripped (block-spans stripped #"(?m)^[ \t]*# cch:(?:begin|end) cch-control-mcp[ \t]*(?:\r?\n|$)"))
                   stripped)
        blocks (map :text (block-spans stripped (block-pattern "cch-control-plane")))
        binding-owned? (and (seq blocks)
                            (every? #(safe-hook-block? % (fn [command]
                                                         (re-find #"(?:^|[/\\])cch['\"]?\s+control\s+bind-codex-source\b" command)))
                                    blocks))]
    {:contents (if binding-owned? (strip-block stripped "cch-control-plane") stripped)
     :mcp-removed? (:owned? info)
     :binding-removed? (boolean binding-owned?)
     :preserved-binding? (boolean (and (not binding-owned?)
                                      (re-find #"(?m)^\s*# cch:begin cch-control-plane\s*$" (structure-text stripped))))
     :preserved-mcp? (and (:present? info) (not (:owned? info)))}))

(defn remove-control-config! [path]
  (let [contents (read-config path)
        result (strip-control-config contents)]
    (when-not (= contents (:contents result)) (write-config! path (:contents result)))
    (dissoc result :contents)))
