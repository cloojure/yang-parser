(ns tst.parse.demo
  (:use parse.core
        parse.transform
        tupelo.test
        clojure.test
        tupelo.x-forest)
  (:require
    [clojure.data :as cd]
    [clojure.java.io :as io]
    [clojure.set :as set]
    [clojure.string :as str]
    [clojure.test.check :as tc]
    [clojure.test.check.clojure-test :as tst]
    [clojure.test.check.generators :as gen]
    [clojure.test.check.properties :as prop]
    [clojure.walk :as walk]
    [instaparse.core :as insta]
    [schema.core :as s]
    [tupelo.core :as t]
    [tupelo.misc :as tm]
    [tupelo.parse :as tp]
    [tupelo.schema :as tsk]
    [tupelo.string :as ts]
    [tupelo.enlive :as te])
)
(t/refer-tupelo)

;*****************************************************************************
(dotest
  (let [abnf-src            "
int           = <ws> digits <ws>  ; ex '123'
digits        = 1*digit           ; 1 or more digits
<digit>       = %x30-39           ; 0-9
<ws>          = 1*' '             ; space: 1 or more
"
        tx-map              {:int    (fn fn-int [arg] [:int (Integer/parseInt arg)])
                             :digits str
                             }
        parse-and-transform (create-parser-transformer abnf-src tx-map)
        ]
    (throws? (parse-and-transform "123xyz"))
    (with-forest (new-forest)
      (is=
        (hid->tree (add-tree-hiccup (parse-and-transform "123")))
        (hid->tree (add-tree-hiccup (parse-and-transform "  123 ")))
        {:attrs {:tag :int}, :content [123]}))))

; If we use the InstaParse built-in ability to perform simple "pre-transforms" on the AST, we can greatly
; simplify out manual transformations.  Compare how simple tx-map is below with the previous example. Also,
; notice that we don't need the (prune-whitespace-nodes ...) function anyplace either.
(dotest
  (let [abnf-src            "
tokens                  = *token <ws>
token                   = <ws> (integer / identifier / string)
identifier              = identifier-start-char *identifier-body-char
integer                 = [ sign ] digits  ; digits with optional sign
string                  = <quote-double> *(ws / vis-char-no-dquote) <quote-double>   ; no escaping quotes yet

<identifier-start-char> = alpha / underscore
<identifier-body-char>  = alpha / underscore / digit / hyphen
<alpha>                 = %x41-5A / %x61-7A     ; A-Z / a-z
<hyphen>                = %x2D  ; - char
<underscore>            = %x5F  ; _ char
<sign>                  = '+' / '-'       ; ignore + or - functions for now
digits                  = 1*digit
<digit>                 = %x30-39         ; 0-9
<ws>                    = 1*' '           ; space: 1 or more
quote-double            = %x22
quote-single            = %x27
vis-char                = %x21-7E ; visible (printing) characters
<vis-char-no-dquote>    = %x21    / %x23-7E ; all visible chars without quote-double
"
        tx-map              {:digits     (fn fn-digits [& args] (str/join args))
                             :integer    (fn fn-integer [& args] [:integer (Integer/parseInt (str/join args))])
                             :identifier (fn fn-identifier [& args] [:identifier (str/join args)])
                             :string     (fn fn-string [& args] [:string (str/join args)])
                             }
        parse-and-transform (create-parser-transformer abnf-src tx-map)
        ]
    (with-forest (new-forest)
      (is= (hid->bush (add-tree-hiccup (parse-and-transform "girl")))
        [{:tag :tokens}
         [{:tag :token}
          [{:tag :identifier} "girl"]]])

      (is= (hid->bush (add-tree-hiccup (parse-and-transform (ts/quotes->double "'abc'"))))
        [{:tag :tokens}
         [{:tag :token}
          [{:tag :string} "abc"]]])

      (is= (hid->bush (add-tree-hiccup (parse-and-transform +123)))
        [{:tag :tokens}
         [{:tag :token}
          [{:tag :integer} 123]]])

      (is= (hid->bush (add-tree-hiccup (parse-and-transform -123)))
        [{:tag :tokens}
         [{:tag :token}
          [{:tag :integer} -123]]])

      ; All together now!
      (is= (hid->bush (add-tree-hiccup
                        (parse-and-transform
                          (ts/quotes->double "do-re-mi abc 1 23 baby 'you and me girl'"))))
        [{:tag :tokens}
         [{:tag :token} [{:tag :identifier} "do-re-mi"]]
         [{:tag :token} [{:tag :identifier} "abc"]]
         [{:tag :token} [{:tag :integer} 1]]
         [{:tag :token} [{:tag :integer} 23]]
         [{:tag :token} [{:tag :identifier} "baby"]]
         [{:tag :token} [{:tag :string} "you and me girl"]]]))))


;-----------------------------------------------------------------------------
(dotest
  (let [abnf-src            "
tokens                  = *token <ws>
token                   = <ws> (integer / identifier / string)
identifier              = identifier-start-char *identifier-body-char
integer                 = [ sign ] digits  ; digits with optional sign
string                  = <quote-double> *(ws / vis-char-no-dquote) <quote-double>   ; no escaping quotes yet

<identifier-start-char> = alpha / underscore
<identifier-body-char>  = alpha / underscore / digit / hyphen / dot
<alpha>                 = %x41-5A / %x61-7A     ; A-Z / a-z
<hyphen>                = %x2D  ; - char
<underscore>            = %x5F  ; _ char
<dot>                   = %x2E  ; . char
<sign>                  = '+' / '-'       ; ignore + or - functions for now
digits                  = 1*digit
<digit>                 = %x30-39         ; 0-9
<ws>                    = 1*' '           ; space: 1 or more
quote-double            = %x22
quote-single            = %x27
vis-char                = %x21-7E ; visible (printing) characters
<vis-char-no-dquote>    = %x21    / %x23-7E ; all visible chars without quote-double
"

        tx-map              {:digits     (fn fn-digits [& args] (str/join args))
                             :sign       no-label
                             :integer    (fn fn-integer [& args] [:integer (Integer/parseInt (str/join args))])
                             :identifier (fn fn-identifier [& args] [:identifier (str/join args)])
                             :string     (fn fn-string [& args] [:string (str/join args)])
                             }
        parse-and-transform (create-parser-transformer abnf-src tx-map)
        ]
    (with-forest (new-forest)
      (is= (hid->bush (add-tree-hiccup (parse-and-transform "girl.2")))
        [{:tag :tokens}
         [{:tag :token}
          [{:tag :identifier} "girl.2"]]]))))


