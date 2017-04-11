(ns parse.core
  (:require
    [clojure.java.io :as io]
    [clojure.set :as set]
    [clojure.string :as str]
    [instaparse.core :as insta]
    [schema.core :as s]
    [tupelo.core :as t]
    [tupelo.misc :as tm]
    [tupelo.schema :as tsk]
    [tupelo.string :as ts]
  ))
(t/refer-tupelo)

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

(defn yang-transform
  [parse-tree]
  (insta/transform
    {
     :string-compound str
     :string-simple   str
     :string          (fn [& args] [:string (str/join args) ] )
     :identifier      (fn [& args] [:identifier (str/join args) ] )
     :iso-year        str

     :boolean         (fn fn-boolean [arg] (java.lang.Boolean. arg))

     :base            (fn fn-base-arg [[_ name]] [:base name])

     :ident-name      (fn fn-name-arg [arg] [:name arg])
     :date-arg        (fn fn-name-arg [arg] [:name arg])
     :namespace       (fn fn-namespace [arg] [:namespace arg])
     :prefix          (fn fn-prefix [arg] [:prefix arg])
     :description     (fn fn-description [arg] [:description (tm/collapse-whitespace arg)])
     :error-message   (fn fn-description [arg] [:error-message (tm/collapse-whitespace arg)])
     :contact         (fn fn-description [arg] [:contact (tm/collapse-whitespace arg)])
     :length          (fn fn-length [arg] [:length arg])

     :enum-simple     (fn fn-enum-simple [& args]
                        [:enum [:name (first args)]])
     :enum-composite  (fn fn-enum-composite [& args]
                        (let [name    (first args)
                              content (rest args)]
                          (t/prepend :enum [:name name] content)))

     :type-simple     (fn fn-type-simple [& args]
                        [:type [:name (first args)]])
     :type-composite  (fn fn-type-composite [& args]
                        (let [name    (first args)
                              content (rest args)]
                          (t/prepend :type [:name name] content)))
     } parse-tree))

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

(defn create-yang-parser
  "Creates & returns a yang parser from the ABNF definition located in `resources/<filename>` "
  [filename]
  (insta/parser (io/resource filename) ; e.g. "yang3.abnf"
    :input-format     :abnf
    ; :auto-whitespace  :standard   ; #todo broken at present
    ; :output-format    :enlive
  ))

(defn -main
  [& args]
  (println "main - enter")
  (doseq [curr-file yang-root-names]
    (newline)
    (let [yang-parser (create-yang-parser "yang2.abnf")
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
