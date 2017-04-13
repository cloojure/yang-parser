(ns tst.parse.demo
  (:use parse.core
        parse.transform
        tupelo.test
        clojure.test)
  (:require
    [clojure.set :as set]
    [clojure.string :as str]
    [clojure.test.check :as tc]
    [clojure.test.check.clojure-test :as tst]
    [clojure.test.check.generators :as gen]
    [clojure.test.check.properties :as prop]
    [instaparse.core :as insta]
    [schema.core :as s]
    [tupelo.core :as t]
    [tupelo.gen :as tgen]
    [tupelo.misc :as tm]
    [tupelo.schema :as tsk]
    [tupelo.string :as ts]
  ))
(t/refer-tupelo)
(t/print-versions)

(def abnf-base "
<text-char>                     = vis-char / char-whitespace
<text-char-no-dquote>           = vis-char-no-dquote / char-whitespace

blk-open                        = <ows> <'{'>
blk-close                       = <ows> <'}'>
term                            = <ows> <';'>
date-arg                        = iso-year
; Note that 4digit is the same as 4*4(digit); both mean exactly 4 digits
iso-year                        = 4digit '-' 2digit '-' 2digit
boolean                         = 'true' / 'false'

ws                              = 1*(<char-whitespace>)   ; whitespace           (1 or more chars)
ows                             =  *(<char-whitespace>)   ; optional whitespace: (0 or more chars)
<char-whitespace-horiz>         = (space / tab)        ; horizontal spacing
<char-whitespace-eol>           = (carriage-return / line-feed)  ; vertical   spacing (EOL chars)
<char-whitespace>               = (char-whitespace-horiz / char-whitespace-eol)        ; any spacing

<carriage-return>               = %x0D
<line-feed>                     = %x0A
<space>                         = %x20  ; space
<tab>                           = %x09  ; horizontal tab
<quote-single>                  = %x27  ; ' char
<quote-double>                  = %x22  ; ' char
<colon>                         = %x3A  ; : char
<dot>                           = %x2E  ; . char
<hyphen>                        = %x2D  ; - char
<underscore>                    = %x5F  ; _ char
<star>                          = %x2A  ; * char
<slash>                         = %x2F  ; / char
<alpha>                         = %x41-5A / %x61-7A     ; A-Z / a-z
<digit>                         = %x30-39   ; 0-9
<digits>                        = 1*digit   ; 1 or more digits
<integer>                       = [ '+' / '-' ] digits  ; digits with optional sign
<hex-digit>                     = digit / %x61 / %x62 / %x63 / %x64 / %x65 / %x66 ; only lower-case a..f
<vis-char>                      = %x21-7E ; visible (printing) characters
<vis-char-no-dquote>            = %x21    / %x23-7E ; all visible chars without quote-double
<vis-char-no-squote>            = %x21-26 / %x28-7E ; all visible chars without quote-single
<vis-char-no-quotes>            = %x21    / %x23-26 / %x28-7E ; all visible chars except both quotes
" )

(def abnf-string "string = <quote-double> *(text-char-no-dquote) <quote-double> " )
(def abnf-identifier "
; An identifier MUST NOT start with (('X'|'x') ('M'|'m') ('L'|'l')) (i.e. 'xml' in any case)
identifier                      = identifier-start-char *identifier-body-char
<identifier-start-char>         = alpha / underscore
<identifier-body-char>          = alpha / underscore / digit / hyphen " )
(dotest
  (let [abnf-src (str abnf-string abnf-base)
        yp       (create-abnf-parser abnf-src)
        s1       (ts/quotes->double "'hello'")
        s1-tree  (yp s1)
        s1-ast   (yang-transform s1-tree)
        ]
    (is= [:string "h" "e" "l" "l" "o"] s1-tree)
    (is= [:string "hello"] s1-ast)
    )
  (let [abnf-src (str abnf-identifier abnf-base)
        yp       (create-abnf-parser abnf-src)
        s1       "name"
        s1-tree  (yp s1)
        s1-ast   (yang-transform s1-tree)]
    (is= [:identifier "n" "a" "m" "e"] s1-tree)
    (is= [:identifier "name"] s1-ast)
    (throws?
      (let [s3      "xml-name"
            s3-tree (yp s3)
            s3-ast  (yang-transform s3-tree)]))))

(dotest
  (let [abnf-tokens "
tokens = *token <ows>   ; can have trailing <ows> ***** ONLY AT THE TOP LEVEL! *****
token  =  <ows> ( identifier / string ) "
        abnf-src (str abnf-tokens abnf-string abnf-identifier abnf-base)
       ; _ (println :abnf-src \newline abnf-src)
        yp       (create-abnf-parser abnf-src)
        s1       (ts/quotes->double "  ident1 ")
        s1-tree  (yp s1)
        s1-ast   (yang-transform s1-tree) ]
    (is= s1-ast
      [:tokens
       [:token [:identifier "i"]]
       [:token [:identifier "d"]]
       [:token [:identifier "e"]]
       [:token [:identifier "n"]]
       [:token [:identifier "t1"]]])))

(def abnf-tokens "
tokens =  <ows> token *( <ws> token) <ows>   ; can have trailing <ows> ***** ONLY AT THE TOP LEVEL! *****
token  =  identifier / string ")
(dotest
  (let [abnf-src    (str abnf-tokens abnf-string abnf-identifier abnf-base)
        yp          (create-abnf-parser abnf-src)
        s1          (ts/quotes->double "  ident1 ")
        s1-tree     (yp s1)
        s1-ast      (yang-transform s1-tree)
        s2          (ts/quotes->double "  ident1 'str1' ident2 'str2' ")
        s2-tree     (yp s2)
        s2-ast      (yang-transform s2-tree)
        ]
    (is= s1-ast [:tokens [:token [:identifier "ident1"]]])
    (is= s2-ast
      [:tokens
       [:token [:identifier "ident1"]]
       [:token [:string     "str1"]]
       [:token [:identifier "ident2"]]
       [:token [:string     "str2"]]] ))

  (let [abnf-src (str abnf-tokens abnf-string abnf-identifier abnf-base)
        yp       (create-abnf-parser abnf-src)]

    (let [s1 (ts/quotes->double "ident1")]
      (check 22 (prop/for-all [s1 (tgen/txt-join (gen/tuple tgen/whitespace (tgen/constantly s1) tgen/whitespace))]
                  (let [s1-tree (yp s1)
                        s1-ast  (yang-transform s1-tree)]
                    (is= s1-ast [:tokens [:token [:identifier "ident1"]]])))))

    (let [s1 (ts/quotes->double "'str1'")]
      (check 22 (prop/for-all [samp (tgen/txt-join (gen/tuple tgen/whitespace (tgen/constantly s1) tgen/whitespace))]
                  (let [samp-tree (yp samp)
                        samp-ast  (yang-transform samp-tree)]
                    (is= samp-ast [:tokens [:token [:string "str1"]]])))))

    (let [s1 (ts/quotes->double "  ident1 'str1' ident2 'str2' ")]
      (check 22 (prop/for-all [samp (tgen/txt-join (gen/tuple tgen/whitespace (tgen/constantly s1) tgen/whitespace))]
                  (let [samp-tree (yp samp)
                        samp-ast  (yang-transform samp-tree)]
                    (is= samp-ast
                          [:tokens
                             [:token [:identifier "ident1"]]
                             [:token [:string     "str1"]]
                             [:token [:identifier "ident2"]]
                             [:token [:string     "str2"]]] )))))
  )
)
