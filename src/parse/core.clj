(ns parse.core
  (use parse.transform)
  (:require
    [clojure.java.io :as io]
    [clojure.set :as set]
    [clojure.string :as str]
    [instaparse.core :as insta]
    [schema.core :as s]
    [tupelo.core :as t]
    [tupelo.x-forest :as tf]
    [tupelo.misc :as tm]
    [tupelo.parse :as tp]
    [tupelo.schema :as tsk]
    [tupelo.string :as ts]
    [tupelo.enlive :as te]))
(t/refer-tupelo)

(defn instaparse-failure? [arg] (instance? instaparse.gll.Failure arg))

(def type-marshal-map {:decimal64 str
                       :int64     str
                       :string    str})

(def type-unmarshal-map {:decimal64 tp/parse-double
                         :int64     tp/parse-long
                         :string    str})

(def rpc-fn-map {:add  (fn fn-add [& args] (apply + args))
                 :mult (fn fn-mult [& args] (apply * args))
                 :pow  (fn fn-power [x y] (Math/pow x y))})

(defn leaf-schema->parser
  [schema]
  (try
    (let [type      (te/get-leaf schema [:leaf :type :identifier]) ; eg "decimal64"
          parser-fn (grab (str->kw type) type-unmarshal-map)]
      parser-fn)
    (catch Exception e
      (throw (RuntimeException. (str "leaf-schema->parser: failed for schema=" schema \newline
                                  "  caused by=" (.getMessage e)))))))

#_(s/defn validate-parse-leaf-hid
  "Validate & parse a leaf msg value given a leaf leaf-schema (Enlive-format)."
  [schema-hid  :- tf/HID
   leaf-hid :- tf/HID ]
  (try
    (assert (= (grab :tag leaf-schema) :leaf))
    (let [leaf-name-schema (keyword (te/get-leaf leaf-schema [:leaf :identifier]))
          leaf-name-val    (grab :tag leaf-val)
          xx              (assert (= leaf-name-schema leaf-name-val))
          ; #todo does not yet verify any attrs;  what rules?
          parser-fn       (leaf-schema->parser leaf-schema)
          parsed-value    (parser-fn (only (grab :content leaf-val)))]
      parsed-value)
    (catch Exception e
      (throw (RuntimeException. (str "validate-parse-leaf-val: failed for leaf-schema=" leaf-schema \newline
                                  "  leaf-val=" leaf-val \newline
                                  "  caused by=" (.getMessage e)))))))
(defn validate-parse-leaf
  "Validate & parse a leaf msg value given a leaf leaf-schema (Enlive-format)."
  [leaf-schema leaf-val]
  (try
    (assert (= (grab :tag leaf-schema) :leaf))
    (let [leaf-name-schema (keyword (te/get-leaf leaf-schema [:leaf :identifier]))
          leaf-name-val    (grab :tag leaf-val)
          xx              (assert (= leaf-name-schema leaf-name-val))
          ; #todo does not yet verify any attrs;  what rules?
          parser-fn       (leaf-schema->parser leaf-schema)
          parsed-value    (parser-fn (only (grab :content leaf-val)))]
      parsed-value)
    (catch Exception e
      (throw (RuntimeException. (str "validate-parse-leaf-val: failed for leaf-schema=" leaf-schema \newline
                                  "  leaf-val=" leaf-val \newline
                                  "  caused by=" (.getMessage e)))))))

(defn validate-parse-rpc-enlive
  "Validate & parse a rpc msg valueue given an rpc rpc-schema (Enlive-format)."
  [rpc-schema rpc-msg]
  (try
    (assert (= :rpc (grab :tag rpc-schema) (grab :tag rpc-msg)))
    (let            ; spy-let-pretty
      [rpc-attrs       (grab :attrs rpc-msg)
       rpc-tag-schema  (keyword (te/get-leaf rpc-schema [:rpc :identifier]))
       rpc-value       (te/get-leaf rpc-msg [:rpc])
       rpc-value-tag   (grab :tag rpc-value)
       rpc-value-attrs (grab :attrs rpc-value)
       xx              (assert (= rpc-tag-schema rpc-value-tag))
       ; #todo does not yet verify any attrs ;  what rules?
       fn-args-schema  (grab :content (te/get-tree rpc-schema [:rpc :input]))
       fn-args-value   (grab :content (te/get-tree rpc-msg [:rpc rpc-value-tag]))
       parsed-args     (mapv validate-parse-leaf fn-args-schema fn-args-value)
       rpc-fn          (grab rpc-value-tag rpc-fn-map)
       rpc-fn-result   (apply rpc-fn parsed-args)
       rpc-result      {:tag     :rpc-reply
                        :attrs   rpc-attrs
                        :content [{:tag     :data
                                   :attrs   {}
                                   :content [rpc-fn-result]}]}]
      rpc-result)
    (catch Exception e
      (throw (RuntimeException. (str "validate-parse-rpc: failed for rpc-schema=" rpc-schema \newline
                                  "  rpc-msg=" rpc-msg \newline
                                  "  caused by=" (.getMessage e)))))))

(defn validate-parse-leaf-tree
  "Validate & parse a leaf msg value given a leaf arg-schema (Enlive-format)."
  [arg-schema arg-val]
  (let
    [arg-name-schema (fetch-in arg-schema [:attrs :tag])
     arg-type-schema (fetch-in arg-schema [:attrs :type])
     arg-name-val    (fetch-in arg-val [:attrs :tag])
     xx              (assert (= arg-name-schema arg-name-val))
     ; #todo does not yet verify any attrs;  what rules?
    parser-fn        (grab arg-type-schema type-unmarshal-map)
    parsed-value    (parser-fn (only (grab :content arg-val)))]
    parsed-value)

  #_(try
    (catch Exception e
      (throw (RuntimeException. (str "validate-parse-arg-val: failed for arg-schema=" arg-schema \newline
                                  "  arg-val=" arg-val \newline
                                  "  caused by=" (.getMessage e))))))
  )

(defn validate-parse-rpc-tree
  "Validate & parse a rpc msg valueue given an rpc schema-hid (Enlive-format)."
  [schema-hid rpc-hid]
  (let
    [rpc-tree (tf/hid->tree rpc-hid)
     schema-tree (tf/hid->tree schema-hid)
     _ (assert (= :rpc
                 (fetch-in schema-tree [:attrs :tag])
                 (fetch-in rpc-tree [:attrs :tag])))
     rpc-attrs       (grab :attrs rpc-tree)
     schema-tag   (only (tf/find-leaf-content schema-hid [:rpc :identifier]))
     rpc-tag      (it-> rpc-tree
                    (grab :kids it)
                    (only it)
                    (fetch-in it [:attrs :tag]))
     _              (assert (= schema-tag rpc-tag))
     ; #todo does not yet verify any attrs ;  what rules?

     fn-args-schema  (grab :kids (tf/find-tree schema-hid [:rpc :input]))
     fn-args-rpc   (grab :kids (tf/find-tree rpc-hid [:rpc rpc-tag]))

     parsed-args     (mapv validate-parse-leaf-tree fn-args-schema fn-args-rpc)
     rpc-fn          (grab rpc-tag rpc-fn-map)
     rpc-fn-result   (apply rpc-fn parsed-args)
     result-hid         (tf/add-node (glue rpc-attrs {:tag :rpc-reply})
                          [(tf/add-leaf {:tag :data} rpc-fn-result)]) ]
    result-hid )

  #_(try
    (catch Exception e
      (throw (RuntimeException. (str "validate-parse-rpc: failed for schema-hid=" (pretty-str schema-hid) \newline
                                  "  rpc-hid=" (pretty-str rpc-hid) \newline
                                  "  caused by=" (.getMessage e))))))
)

(def yang-root-names ; #todo
 [   "acme"
     "toaster"
     "turing-machine"
     "ietf-netconf-acm"
     "brocade-beacon"
     "brocade-rbridge"
     "brocade-terminal"
     "yuma-proc"
     "yuma-xsd"
   ])

(defn space-wrap [text] (str \space text \space))

(defn create-abnf-parser-raw
  "Given an ABNF syntax string, creates & returns a parser"
  [abnf-str]
  (let [root-parser    (insta/parser abnf-str :input-format :abnf)
        wrapped-parser (fn fn-wrapped-parser [yang-src]
                         (let [parse-result (try
                                              (root-parser yang-src)
                                              (catch Throwable e ; unlikely
                                                (throw (RuntimeException.
                                                         (str "root-parser: InstaParse failed for \n"
                                                           "yang-src=[[" (clip-str 222 yang-src) "]] \n"
                                                           "caused by=" (.getMessage e))))))]
                           (if (instaparse-failure? parse-result) ; This is the normal failure path
                             (throw (RuntimeException.
                                      (str  "root-parser: InstaParse failed for \n "
                                            "yang-src=[[" (clip-str 222 yang-src) "]] \n"
                                            "caused by=[[" (pr-str parse-result) "]]" )))
                             parse-result)))]
    wrapped-parser))

(defn create-abnf-parser
  "Given an ABNF syntax string, creates & returns a parser that wraps the yang source
  with a leading and trailing space."
  [abnf-str]
  (let [parser-raw           (create-abnf-parser-raw abnf-str)
        space-wrapped-parser (fn fn-space-wrapped-parser [yang-src]
                               (parser-raw (space-wrap yang-src)))]
    space-wrapped-parser))

(defn create-parser-transformer
  "Given an ABNF syntax string, creates & returns a parser that wraps the yang source
  with a leading and trailing space."
  [abnf-str tx-map]
  (let [parser-raw           (create-abnf-parser-raw abnf-str)
        space-wrapped-parser (fn fn-space-wrapped-parser [yang-src]
                               (parser-raw (space-wrap yang-src)))
        parse-and-transform  (fn fn-parse-and-transform [src-text]
                               (let [ast-parse (space-wrapped-parser src-text)
                                     ast-tx    (insta/transform tx-map ast-parse)]
                                 (when (instaparse-failure? ast-tx)
                                   (throw (IllegalArgumentException. (str ast-tx))))
                                 ast-tx))]
    parse-and-transform))

(s/defn leaf-name->attrs
  [leaf-hid :- tf/HID]
  (let [name-kw (keyword (only (tf/find-leaf-content leaf-hid [:leaf :identifier])))
        hid-remove (tf/find-hids leaf-hid [:leaf :identifier])]
    (tf/merge-attrs leaf-hid {:name name-kw})
    (tf/remove-kids leaf-hid hid-remove)))

(s/defn leaf-type->attrs
  [leaf-hid :- tf/HID]
  (tf/hid->tree leaf-hid)
  (let [type-kw (keyword (only (tf/find-leaf-content leaf-hid [:leaf :type :identifier])))
        hid-remove (tf/find-hids leaf-hid [:leaf :type])]
    (tf/merge-attrs leaf-hid {:type type-kw})
    (tf/remove-kids leaf-hid hid-remove)))

(s/defn tx-leaf-type-ident
  "Within a [:leaf ...] node, convert [:type [:identifier 'decimal64']] ->
    {:type :decimal64} "
  [rpc-hid :- tf/HID]
  (tf/validate-hid rpc-hid)
  (let [rpc-leaf-paths (tf/find-paths rpc-hid [:rpc :* :leaf])
        rpc-leaf-hids  (mapv last rpc-leaf-paths) ]
    (run! leaf-type->attrs rpc-leaf-hids)
    (run! leaf-name->attrs rpc-leaf-hids)
    (doseq [hid rpc-leaf-hids]
      (tf/remove-all-kids hid))))

(s/defn tx-rpc
  [rpc-hid]
  (tf/validate-hid rpc-hid)
  (tx-leaf-type-ident rpc-hid)
  (let [id-hid (tf/find-hid rpc-hid [:rpc :identifier])
        desc-hid (tf/find-hid rpc-hid [:rpc :description])
        rpc-name (keyword (only (tf/leaf->content id-hid)))]
    (tf/merge-attrs rpc-hid {:name rpc-name})
    (tf/remove-kids rpc-hid [id-hid desc-hid]))
  )

(s/defn rpc->api :- [s/Any]
  [rpc-hid :- tf/HID]
  (let [rpc-tree           (tf/hid->tree rpc-hid)
        rpc-name           (kw->str (fetch-in rpc-tree [:attrs :name]))
        rpc-input-hid      (tf/find-hid rpc-hid [:rpc :input])
        rpc-input-arg-hids (grab :kids (tf/hid->node rpc-input-hid))
        rpc-arg-syms       (forv [hid rpc-input-arg-hids]
                             (it-> hid
                               (tf/hid->tree it)
                               (fetch-in it [:attrs :name])
                               (kw->sym it)))
        fn-name            (symbol (str "fn-" rpc-name))
        fn-name-impl       (symbol (str fn-name "-impl"))
        fn-def             (vec->list
                             (-> '(fn)
                               (append fn-name rpc-arg-syms)
                               (append (vec->list (prepend fn-name-impl rpc-arg-syms))))) ]
    fn-def ))


(s/defn rpc-marshall :- s/Any
  [rpc-hid :- tf/HID
   args :- [s/Any]]
  (let [rpc-tree (tf/hid->tree rpc-hid)
        rpc-name (fetch-in rpc-tree [:attrs :name])
        rpc-input-hid (tf/find-hid rpc-hid [:rpc :input])
        rpc-input-arg-hids (grab :kids (tf/hid->node rpc-input-hid))
        _ (assert (= (count args) (count rpc-input-arg-hids)))
        marshalled-args (forv [[hid arg] (mapv vector rpc-input-arg-hids args)]
                          (let [arg-tree (tf/hid->tree hid)
                                arg-name-kw (fetch-in arg-tree [:attrs :name])
                                arg-name-sym (kw->sym arg-name-kw)
                                arg-type (fetch-in arg-tree [:attrs :type])
                                marshal-fn (type-marshal-map arg-type)
                                marshalled-arg [arg-name-kw (marshal-fn arg)]
                                ]
                            marshalled-arg))
        msg-hiccup [:rpc (glue [rpc-name {:xmlns "my-own-ns/v1"}] marshalled-args)]]
    msg-hiccup))

(defn rpc-unmarshall-args
  [schema-arg-trees msg-arg-trees]
  (let [args (map-with [schema-tree schema-arg-trees
                        msg-tree msg-arg-trees]
               (let [schema-arg-name (fetch-in schema-tree [:attrs :name])
                     schema-arg-type (fetch-in schema-tree [:attrs :type])
                     schema-arg-parse-fn (grab schema-arg-type type-unmarshal-map)
                     msg-arg-name (fetch-in msg-tree [:attrs :tag])
                     _ (assert (= msg-arg-name schema-arg-name))
                     msg-arg-value-raw (only (grab :content msg-tree))
                     msg-arg-value (schema-arg-parse-fn msg-arg-value-raw)]
                 msg-arg-value))]
    args))

(s/defn rpc-unmarshall :- s/Any
  [schema-hid
   msg :- [s/Any] ]
  (let
    [msg-hid (tf/add-tree-hiccup msg)
     msg-tree (tf/hid->tree msg-hid)
     schema-tree (tf/hid->tree schema-hid)
     _ (assert (= :rpc
                 (fetch-in schema-tree [:attrs :tag])
                 (fetch-in msg-tree [:attrs :tag])))
     schema-fn-name (fetch-in schema-tree [:attrs :name])
     msg-call (it-> msg-tree
                (grab :kids it)
                (only it))
     msg-fn-name (fetch-in msg-call [:attrs :tag])
     _ (assert (= schema-fn-name msg-fn-name))
     schema-input-hid (tf/find-hid schema-hid [:rpc :input])
     schema-input-hids (grab :kids (tf/hid->node schema-input-hid))
     schema-arg-trees (mapv tf/hid->tree schema-input-hids)
     msg-arg-trees (grab :kids msg-call)
     _ (assert (= (count schema-arg-trees) (count msg-arg-trees) ))
     args (rpc-unmarshall-args schema-arg-trees msg-arg-trees)
     rpc-fn (grab msg-fn-name rpc-fn-map)
     rpc-unmarshalled-map {:rpc-fn rpc-fn
                           :args args} ]
    rpc-unmarshalled-map))

(defn invoke-rpc [rpc-unmarshalled-map]
  (let [rpc-fn (grab :rpc-fn rpc-unmarshalled-map)
        args (grab :args rpc-unmarshalled-map)
        rpc-result (apply rpc-fn args) ]
    rpc-result) )


; #todo kill this old stuff -> pre-parse to remove comments
;---------------------------------------------------------------------------------------------------
; #todo #awt add schema stuff
; #todo replace tsk/List -> [Character]
; #todo replace txt -> charseq

(s/defn starts-with-eol :- s/Bool
  "Returns true if arg begins with one of CR or LF or FF characters"
  [txt :- s/Any]
  (contains? ts/chars-whitespace-eol (first txt)))

(s/defn found-comment-cpp-start :- s/Bool
  "Returns true if text begins with a C++ style comment (e.g  '// This is a comment')"
  [txt :- s/Any]
  (t/starts-with? txt "//"))

(s/defn consume-comment-cpp :- tsk/List
  "Given text beginning with a C++ style comment (e.g  '// This is a comment'), consumes
  characters until EOL (either CR or LF). Does not consume any EOL chars. Returns all chars
  beginning with EOL."
  [txt :- s/Any]
  (assert (found-comment-cpp-start txt))
  (let [[txt-comment txt-remaining] (t/split-using starts-with-eol txt)]
    txt-remaining))

(s/defn found-comment-c-start :- s/Bool
  "Returns true if text begins with a C style comment (e.g  '/* This is a comment */')"
  [txt :- s/Any]
  (t/starts-with? txt "/*"))

(s/defn found-comment-c-stop :- s/Bool
  "Returns true if text begins with a C style comment terminator '*/' (e.g  '/* This was a comment */')"
  [txt :- s/Any]
  (t/starts-with? txt "*/"))

(s/defn consume-comment-c :- tsk/List
  "Given text beginning with a C style comment (e.g  '/* This is a comment */'), consumes
  characters from beginning to end of comment (inclusive). Returns all chars
  after the comment terminator '*/'"
  [orig :- s/Any]
  (assert (found-comment-c-start orig))
  (let [search-str (drop 2 orig) ; drop the initial "/*" chars
        [chars-comment chars-remaining] (t/split-match search-str "*/")
        result     (drop 2 chars-remaining)]
    result ))

; #todo does not handle escaped double-quote
(s/defn starts-with-dquote :- s/Bool
  "Returns true if text begins with a double-quote char 0x22  "
  [txt :- s/Any]
  (= \" (first txt)))

(s/defn save-dquote-string :- tsk/KeyMap
  "Saves a double-quoted string from :src to :result in context map."
  [ctx-map :- tsk/KeyMap]
  (let [src             (grab :src ctx-map)
        result          (grab :result ctx-map)
        [chars-quoted chars-remaining]
        (t/split-using starts-with-dquote (rest src) )
        new-result      (t/glue result ; existing result
                          [\"] chars-quoted [\"] ) ; newly-found quoted str
        new-src         (rest chars-remaining) ; drop terminating quote
        new-ctx-map     {:result new-result
                         :src    new-src} ]
    new-ctx-map))

; #todo does not handle escaped single-quote
(s/defn starts-with-squote :- s/Bool
  "Returns true if text begins with a single-quote char 0x27  "
  [txt :- s/Any]
  (= \' (first txt)))

(s/defn save-squote-string :- tsk/KeyMap
  "Saves a single-quoted string from :src to :result in context map."
  [ctx-map :- tsk/KeyMap]
  (let [src             (grab :src ctx-map)
        [chars-quoted chars-remaining]
        (t/split-using starts-with-squote (rest src) )
        new-result      (t/glue (grab :result ctx-map) ; existing result
                          [\'] chars-quoted [\'] ) ; newly-found quoted str
        new-src         (rest chars-remaining) ; drop terminating quote
        new-ctx-map     {:result new-result
                         :src    new-src} ]
    new-ctx-map))

(s/defn remove-comments :- s/Str
  "Removes both C and C++ style comments from text, ignoring comments embedded in
  simple quoted strings (single- or double-quoted)"
  [orig :- s/Str]
  (let [ctx (atom {:result []
                   :src    (vec orig) } ) ]
    ; #todo add (s/validate (-> ctx :result-chars) s/Char)
    (while (pos? (count (grab :src @ctx)))
      (let [src (grab :src @ctx)]
        (cond
          (found-comment-cpp-start src)
          (let [new-src (consume-comment-cpp src) ]
            (swap! ctx assoc :src new-src))

          (found-comment-c-start src)
          (let [new-src (consume-comment-c src) ]
            (swap! ctx assoc :src new-src))

          (starts-with-dquote src)
          (let [new-ctx-map (save-dquote-string @ctx) ]
            (reset! ctx new-ctx-map))

          (starts-with-squote src)
          (let [new-ctx-map (save-squote-string @ctx) ]
            (reset! ctx new-ctx-map))

          :normal-text
          (let [curr-char (first src)
                new-ctx-map {:result (t/append (grab :result @ctx) curr-char)
                             :src    (drop 1 src)}]
            (reset! ctx new-ctx-map)))))
    (str/join (grab :result @ctx))))

;-----------------------------------------------------------------------------
; #todo kill or keep main?
(defn -main
  [& args]
  (println "main - enter")
  (let [abnf-src            (io/resource "yang2.abnf")
        parse-and-transform (create-parser-transformer abnf-src yang-tx-map)]
    (doseq [curr-file yang-root-names]
      (newline)
      (let [file-in         (str "resources/" curr-file ".yang")
            file-tx         (str "resources/tx/" curr-file ".edn")
            data-in         (slurp file-in)
            _               (do (printf "read:     %s  (chars %d) \n" file-in (count data-in)) (flush))
            _               (print "  removing comments:  ")
            data-nocom      (time (remove-comments data-in))
            _               (print "  parsing:            ")
            yang-ast-hiccup (parse-and-transform data-nocom)
            ]
        (printf "creating: %s \n" file-tx) (flush)
        (spit file-tx (t/pretty-str yang-ast-hiccup))))))


