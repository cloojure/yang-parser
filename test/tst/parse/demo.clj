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

(dotest
  (let [abnf-str "string = <quote-double> *(text-char-no-dquote) <quote-double> "
        abnf-src (str abnf-str abnf-base)
        yp       (create-abnf-parser abnf-src)
        s1       (ts/quotes->double "'hello'")
        s1-tree  (yp s1)
        s1-ast   (yang-transform s1-tree)
        ]
    (is= [:string "h" "e" "l" "l" "o"] (spyx s1-tree))
    (is= [:string "hello"] (spyx s1-ast))
    )
  (let [abnf-str "
identifier                      = identifier-start-char *identifier-body-char
<identifier-start-char>         = alpha / underscore
<identifier-body-char>          = alpha / underscore / digit / hyphen
"
        abnf-src (str abnf-str abnf-base)
        yp       (create-abnf-parser abnf-src)
        s1       "name"
        s1-tree  (spyx (yp s1))
        s1-ast   (yang-transform s1-tree) ]
    (is= [:identifier "n" "a" "m" "e"] (spyx s1-tree))
    (is= [:identifier "name"] (spyx s1-ast))
    (throws?
      (let [s3      "xml-name"
            s3-tree (spyx (yp s3))
            s3-ast  (spyx (yang-transform s3-tree))] ))
  )
)
