(ns parse.core
  (use parse.transform)
  (:require
    [clojure.java.io :as io]
    [clojure.set :as set]
    [clojure.string :as str]
    [instaparse.core :as insta]
    [schema.core :as s]
    [tupelo.core :as t]
    [tupelo.misc :as tm]
    [tupelo.parse :as tp]
    [tupelo.schema :as tsk]
    [tupelo.string :as ts]
  ))
(t/refer-tupelo)

(defn instaparse-failure? [arg] (instance? instaparse.gll.Failure arg))

(def parser-map {"decimal64"  tp/parse-double
                 "int64"      tp/parse-long
                 "string"     str })

(def rpc-fn-map {:add  (fn fn-add [& args] (apply + args))
                 :mult (fn fn-mult [& args] (apply * args))
                 :pow  (fn fn-power [x y] (Math/pow x y))})

(defn leaf-schema->parser
  [schema]
  (try
    (let [type      (get-leaf schema [:leaf :type :identifier]) ; eg "decimal64"
          parser-fn (grab type parser-map)]
      parser-fn)
    (catch Exception e
      (throw (RuntimeException. (str "leaf-schema->parser: failed for schema=" schema \newline
                                  "  caused by=" (.getMessage e)))))))

(defn validate-parse-leaf
  "Validate & parse a leaf msg value given a leaf leaf-schema (Enlive-format)."
  [leaf-schema leaf-val]
  (try
    (assert (= (grab :tag leaf-schema) :leaf))
    (let [leaf-name-schema (keyword (get-leaf leaf-schema [:leaf :identifier]))
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

(defn validate-parse-rpc
  "Validate & parse a rpc msg valueue given an rpc rpc-schema (Enlive-format)."
  [rpc-schema rpc-msg]
  (try
    (assert (= :rpc (grab :tag rpc-schema) (grab :tag rpc-msg)))
    (let [rpc-attrs       (grab :attrs rpc-msg)
          rpc-tag-schema  (keyword (get-leaf rpc-schema [:rpc :identifier]))
          rpc-value       (get-leaf rpc-msg [:rpc])
          rpc-value-tag   (grab :tag rpc-value)
          rpc-value-attrs (grab :attrs rpc-value)
          xx              (assert (= rpc-tag-schema rpc-value-tag))
          ; #todo does not yet verify any attrs ;  what rules?
          fn-args-schema  (grab :content (get-tree rpc-schema [:rpc :input]))
          fn-args-value   (grab :content (get-tree rpc-msg [:rpc rpc-value-tag]))
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
      (throw (RuntimeException. (str "validate-parse-leaf: failed for rpc-schema=" rpc-schema \newline
                                  "  rpc-msg=" rpc-msg \newline
                                  "  caused by=" (.getMessage e)))))))

;---------------------------------------------------------------------------------------------------
(s/def chars-visible-no-dquote :- tsk/Set
  "All visible (printing) ASCII chars except double-quote."
  (set/difference ts/chars-visible #{\"}))

(s/def chars-visible-no-squote :- tsk/Set
  "All visible (printing) ASCII chars except double-quote."
  (set/difference ts/chars-visible #{\'}))

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

(defn create-abnf-parser
  "Given an ABNF syntax string, creates & returns a parser"
  [abnf-str]
  (let [root-parser    (insta/parser abnf-str :input-format :abnf)
        wrapped-parser (fn fn-wrapped-parser [yang-src]
                         (let [parse-result (try
                                              (root-parser yang-src)
                                              (catch Throwable e ; unlikely
                                                (throw (RuntimeException.
                                                         (str "root-parser: InstaParse failed for \n"
                                                           "yang-src=[[" (clip-str 99 yang-src) "]] \n"
                                                           "caused by=" (.getMessage e))))))]
                           (if (instaparse-failure? parse-result) ; This is the normal failure path
                             (throw (RuntimeException.
                                      (str  "root-parser: InstaParse failed for \n "
                                            "yang-src=[[" (clip-str 99 yang-src) "]] \n"
                                            "caused by=[[" (pr-str parse-result) "]]" )))
                             parse-result)))]
    wrapped-parser))

(defn -main
  [& args]
  (println "main - enter")
  (doseq [curr-file yang-root-names]
    (newline)
    (let [yang-parser (create-abnf-parser (io/resource "yang2.abnf"))
          file-in     (str "resources/"       curr-file ".yang")
          file-tx     (str "resources/tx/"    curr-file ".edn")
          data-in     (slurp file-in)
          _           (do (printf "read:     %s  (chars %d) \n" file-in (count data-in)) (flush))
          _           (print "  removing comments:  ")
          data-nocom  (time (remove-comments data-in))
          _           (print "  parsing:            ")
          data-parsed (time (yang-parser data-nocom))
          _           (print "  transform:          ")
          data-tx     (time (yang-transform data-parsed))
          ]
      (printf "creating: %s \n" file-tx) (flush)
      (spit file-tx (t/pretty-str data-tx))
    )))
