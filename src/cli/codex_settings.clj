(ns cli.codex-settings
  "Sentinel-block read/modify/write of ~/.codex/config.toml.

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
            [clojure.string :as str]))

(defn codex-config-path
  "Path to the user's Codex config.toml."
  []
  (str (System/getProperty "user.home") "/.codex/config.toml"))

(defn read-config
  "Read config file as a string. Returns \"\" if missing."
  [path]
  (if (fs/exists? path)
    (slurp path)
    ""))

(defn write-config!
  "Write contents atomically (write to tmp, rename)."
  [path contents]
  (let [dir (fs/parent path)]
    (when-not (fs/exists? dir)
      (fs/create-dirs dir))
    (let [tmp (str path ".tmp")]
      (spit tmp contents)
      (fs/move tmp path {:replace-existing true}))))

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

(defn strip-block
  "Remove the named cch block from `contents`. No-op if absent."
  [contents name]
  (str/replace contents (block-pattern name) ""))

(defn strip-all-blocks
  "Remove every cch sentinel block from `contents`."
  [contents]
  (str/replace contents any-block-pattern ""))

(defn upsert-block
  "Return new contents with the named cch block replaced (or appended).
  Always separates from prior content by a blank line."
  [contents name entries]
  (let [stripped (strip-block contents name)
        sep (cond
              (str/blank? stripped) ""
              (str/ends-with? stripped "\n\n") ""
              (str/ends-with? stripped "\n") "\n"
              :else "\n\n")]
    (str stripped sep (render-block name entries))))

(defn install-hook!
  "Atomically install (or replace) the cch block for `name` in the
  Codex config at `path`. `entries` as in `render-block`."
  [path name entries]
  (let [updated (upsert-block (read-config path) name entries)]
    (write-config! path updated)
    updated))

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
