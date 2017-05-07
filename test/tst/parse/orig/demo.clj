(ns tst.parse.orig.demo
  (:use parse.core
        parse.transform
        tupelo.test
        clojure.test)
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
    [tupelo.gen :as tgen]
    [tupelo.misc :as tm]
    [tupelo.parse :as tp]
    [tupelo.schema :as tsk]
    [tupelo.string :as ts]
    )
  (:import [java.util.concurrent TimeoutException]
           [java.util List]))
(t/refer-tupelo)

;*****************************************************************************
(dotest
  (let [abnf-src            "
int           = digits          ; ex '123'
digits        = 1*digit         ; 1 or more digits
digit         = %x30-39         ; 0-9
delim         = %x20            ; space or semicolon
"
        tx-map              {:int    (fn fn-int [arg] [:int (Integer/parseInt arg)])
                             :digit  no-label
                             :digits str
                             }

        parser              (insta/parser abnf-src :input-format :abnf)
        parse-and-transform (fn [text]
                              (let [result (insta/transform tx-map
                                             (parser text))]
                                (if (instaparse-failure? result)
                                  (throw (IllegalArgumentException. (str result)))
                                  result)))
        ]
    (is= [:int 123] (parse-and-transform "123"))
    (throws? (parse-and-transform "123xyz"))
    (throws? (parse-and-transform " 123  "))
    ))

;*****************************************************************************
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
")

(def abnf-string "string = <quote-double> *(text-char-no-dquote) <quote-double> ")
(def abnf-identifier "
; An identifier MUST NOT start with (('X'|'x') ('M'|'m') ('L'|'l')) (i.e. 'xml' in any case)
identifier                      = identifier-start-char *identifier-body-char
<identifier-start-char>         = alpha / underscore
<identifier-body-char>          = alpha / underscore / digit / hyphen ")
(dotest
  (let [abnf-src (str abnf-string abnf-base)
        yp       (create-abnf-parser-raw abnf-src)
        s1       (ts/quotes->double "'hello'")
        s1-tree  (yp s1)
        s1-ast   (yang-transform s1-tree)
        ]
    (is= [:string "h" "e" "l" "l" "o"] s1-tree)
    (is= [:string "hello"] s1-ast))
  (let [abnf-src (str abnf-identifier abnf-base)
        yp       (create-abnf-parser-raw abnf-src)
        s1       "name"
        s1-tree  (yp s1)
        s1-ast   (yang-transform s1-tree)]
    (is= [:identifier "n" "a" "m" "e"] s1-tree)
    (is= [:identifier "name"] s1-ast)
    (throws? (let [s3      "xml-name"
                   s3-tree (yp s3)
                   s3-ast  (yang-transform s3-tree)]))))

(dotest
  (let [abnf-tokens "
tokens = *token <ows>   ; can have trailing <ows> ***** ONLY AT THE TOP LEVEL! *****
token  =  <ows> ( identifier / string ) "
        abnf-src    (str abnf-tokens abnf-string abnf-identifier abnf-base)
        ; _ (println :abnf-src \newline abnf-src)
        yp          (create-abnf-parser abnf-src)
        s1          (ts/quotes->double "  ident1 ")
        s1-tree     (yp s1)
        s1-ast      (yang-transform s1-tree)]
    (is= s1-ast
      [:tokens
       [:token [:identifier "i"]]
       [:token [:identifier "d"]]
       [:token [:identifier "e"]]
       [:token [:identifier "n"]]
       [:token [:identifier "t1"]]])))

; identifier  = DQUOTE *ALPHA DQUOTE
; ALPHA       = %x41-5A / %x61-7A     ; A-Z / a-z
;
; "cat" =>  [:IDENTIFIER [:DQUOTE "\""] [:ALPHA "c"] [:ALPHA "a"] [:ALPHA "t"] [:DQUOTE "\""] ]


(def abnf-tokens "
tokens =  <ows> token *( <ws> token) <ows>   ; can have trailing <ows> ***** ONLY AT THE TOP LEVEL! *****
token  =  identifier / string ")
(dotest
  (let [abnf-src (str abnf-tokens abnf-string abnf-identifier abnf-base)
        yp       (create-abnf-parser abnf-src)
        s1       (ts/quotes->double "  ident1 ")
        s1-tree  (yp s1)
        s1-ast   (yang-transform s1-tree)
        s2       (ts/quotes->double "  ident1 'str1' ident2 'str2' ")
        s2-tree  (yp s2)
        s2-ast   (yang-transform s2-tree)]
    (is= s1-ast [:tokens [:token [:identifier "ident1"]]])
    (is= s2-ast
      [:tokens
       [:token [:identifier "ident1"]]
       [:token [:string "str1"]]
       [:token [:identifier "ident2"]]
       [:token [:string "str2"]]]))

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
                       [:token [:string "str1"]]
                       [:token [:identifier "ident2"]]
                       [:token [:string "str2"]]])))))))

(dotest
  (let [abnf-src  (io/resource "yang3.abnf")
        yp        (create-abnf-parser abnf-src)
        yang-src  (ts/quotes->double "
module toaster {
  namespace 'http://netconfcentral.org/ns/toaster' ;
  prefix toast;
  organization 'Netconf Central';

  contact
      'Andy Bierman <andy@netconfcentral.org>'  ;

  description 'YANG version of the TOASTER-MIB.';
  revision 2009-11-20{
      description 'Toaster module in progress.'; }
  identity toast-type {
      description 'Base for all bread types supported by the toaster.';
  }
  identity white-bread { base toast:toast-type; description 'White bread.'; }
  typedef DisplayString {
      type string {
          length '0 .. 255';
      }
      description 'YANG version of the SMIv2 DisplayString TEXTUAL-CONVENTION.';
      reference 'RFC 2579, section 2.';
  }
  container toaster{presence 'Indicates the toaster service is available' ;
      description 'Top-level container for all toaster database objects.';
      leaf toasterManufacturer {
          type DisplayString;
          config false;
          mandatory true;
          description 'The name of the manufacturer, for instance:
                   Microsoft Toaster.';
      }
  }


} ")
        yang-tree (yp yang-src)
        yang-ast  (yang-transform yang-tree)]
    (is= yang-ast
      [:module
       [:identifier "toaster"]
       [:namespace [:string "http://netconfcentral.org/ns/toaster"]]
       [:prefix  "toast"]
       [:organization [:string "Netconf Central"]]
       [:contact [:string "Andy Bierman <andy@netconfcentral.org>"]]
       [:description [:string "YANG version of the TOASTER-MIB."]]
       [:revision
        [:iso-date "2009-11-20"]
        [:description [:string "Toaster module in progress."]]]
       [:identity
        [:identifier "toast-type"]
        [:description [:string "Base for all bread types supported by the toaster."]]]
       [:identity
        [:identifier "white-bread"]
        [:base [:identifier "toast:toast-type"]]
        [:description [:string "White bread."]]]
       [:typedef
        [:identifier "DisplayString"]
        [:type [:identifier "string"] [:length [:string "0 .. 255"]]]
        [:description [:string "YANG version of the SMIv2 DisplayString TEXTUAL-CONVENTION."]]
        [:reference [:string "RFC 2579, section 2."]]]
       [:container
        [:identifier "toaster"]
        [:presence [:string "Indicates the toaster service is available"]]
        [:description [:string "Top-level container for all toaster database objects."]]
        [:leaf
         [:identifier "toasterManufacturer"]
         [:type [:identifier "DisplayString"]]
         [:config [:boolean false]]
         [:mandatory [:boolean true]]
         [:description [:string "The name of the manufacturer, for instance: Microsoft Toaster."]]]]
       ]
      )))

(dotest
  (let [abnf-src  (io/resource "yang3.abnf")
        yp        (create-abnf-parser abnf-src)
        yang-src  (ts/quotes->double "
module toaster {
  container toaster{presence 'Indicates the toaster service is available' ;
    description 'Top-level container for all toaster database objects.';
    leaf toasterManufacturer {
        type DisplayString;
        config false;
        mandatory true;
        description 'The name of the manufacturer, for instance:
                 Microsoft Toaster.';
    }
    leaf toasterStatus {
      type enumeration {
        enum up {
          value 1;
          description
            'The toaster knob position is up.
            No toast is being made now.';
        }
        enum down {
          value 2;
          description
            'The toaster knob position is down.
            Toast is being made now.';

        }
      }
      config false;
      mandatory true;
      description
        'This variable indicates the current state of
              the toaster.';
    }
  }
} ")
        yang-tree (yp yang-src)
        yang-ast  (yang-transform yang-tree)]
    (is= yang-ast
      [:module
       [:identifier "toaster"]
       [:container
        [:identifier "toaster"]
        [:presence [:string "Indicates the toaster service is available"]]
        [:description [:string "Top-level container for all toaster database objects."]]
        [:leaf
         [:identifier "toasterManufacturer"]
         [:type [:identifier "DisplayString"]]
         [:config [:boolean false]]
         [:mandatory [:boolean true]]
         [:description [:string "The name of the manufacturer, for instance: Microsoft Toaster."]]]
        [:leaf
         [:identifier "toasterStatus"]
         [:type
          [:identifier "enumeration"]
          [:enum
           [:name [:identifier "up"]]
           [:value [:integer 1]]
           [:description [:string "The toaster knob position is up. No toast is being made now."]]]
          [:enum
           [:name [:identifier "down"]]
           [:value [:integer 2]]
           [:description [:string "The toaster knob position is down. Toast is being made now."]]]]
         [:config [:boolean false]]
         [:mandatory [:boolean true]]
         [:description [:string "This variable indicates the current state of the toaster."]]]
        ]])))

(dotest
  (let [abnf-src  (io/resource "yang3.abnf")
        yp        (create-abnf-parser abnf-src)
        yang-src  (ts/quotes->double "
module toaster {
  rpc make-toast {
    description 'Make some toast. ';
    input {
      leaf toasterDoneness {
        type uint32 { range '1 .. 10' ; }
        default '5';
        description 'This variable controls how well-done is the ensuing toast. ';
      }
      leaf toasterToastType {
          type identityref {
              base toast:toast-type;
          }
          default 'toast:wheat-bread';
          description
            'This variable informs the toaster of the type of the required doneness.';
      }
    }
  }

  rpc cancel-toast {
    description 'Stop making toast, if any is being made. '; }


} ")
        yang-tree (yp yang-src)
        yang-ast  (yang-transform yang-tree)]
    (is= yang-ast
      [:module
       [:identifier "toaster"]
       [:rpc
        [:identifier "make-toast"]
        [:description [:string "Make some toast."]]
        [:input
         [:leaf
          [:identifier "toasterDoneness"]
          [:type
           [:identifier "uint32"]
           [:range [:range-simple [:string "1 .. 10"]]]]
          [:default [:string "5"]]
          [:description
           [:string
            "This variable controls how well-done is the ensuing toast."]]]
         [:leaf
          [:identifier "toasterToastType"]
          [:type
           [:identifier "identityref"]
           [:base [:identifier "toast:toast-type"]]]
          [:default [:string "toast:wheat-bread"]]
          [:description [:string "This variable informs the toaster of the type of the required doneness."]]]]]
       [:rpc
        [:identifier "cancel-toast"]
        [:description [:string "Stop making toast, if any is being made."]]]])))

(dotest
  (let [abnf-src  (io/resource "yang3.abnf")
        yp        (create-abnf-parser abnf-src)
        yang-src  (ts/quotes->double "
module toaster {
  notification toastDone {
    description 'Indicates that the toast in progress has completed.';
    leaf toastStatus {
      type enumeration {
        enum done         { description 'The toast is done.'; }
        enum cancelled    { description 'The toast was cancelled.'; }
        enum error        { description 'The toaster service was disabled or the toaster is broken.'; }
      }
      description 'Indicates the final toast status';
    }
  }
} ")
        yang-tree (yp yang-src)
        yang-ast  (yang-transform yang-tree)]
    (is= yang-ast
      [:module
       [:identifier "toaster"]
       [:notification
        [:identifier "toastDone"]
        [:description
         [:string "Indicates that the toast in progress has completed."]]
        [:leaf
         [:identifier "toastStatus"]
         [:type
          [:identifier "enumeration"]
          [:enum
           [:name [:identifier "done"]]
           [:description [:string "The toast is done."]]]
          [:enum
           [:name [:identifier "cancelled"]]
           [:description [:string "The toast was cancelled."]]]
          [:enum
           [:name [:identifier "error"]]
           [:description [:string "The toaster service was disabled or the toaster is broken."]]]]
         [:description [:string "Indicates the final toast status"]]]]
       ])))


(dotest
  (let [abnf-src            "
size-val      = int / int-px
int           = digits          ; ex '123'
int-px        = digits <'px'>   ; ex '123px'
<digits>      = 1*digit         ; 1 or more digits
<digit>       = %x30-39         ; 0-9
"
        tx-map              {:int      (fn fn-int [& args] [:int (Integer/parseInt (str/join args))])
                             :int-px   (fn fn-int-px [& args] [:int-px (Integer/parseInt (str/join args))])
                             :size-val identity
                             }

        parser              (insta/parser abnf-src :input-format :abnf)
        parse-and-transform (fn [text]
                              (let [result (insta/transform tx-map
                                             (parser text))]
                                (if (instaparse-failure? result)
                                  (throw (IllegalArgumentException. (str result)))
                                  result)))
        ]
    (is= [:int 123] (parse-and-transform "123"))
    (is= [:int-px 123] (parse-and-transform "123px"))
    (throws? (parse-and-transform "123xyz"))))

;-----------------------------------------------------------------------------
; Q: how do we know "123" is not a sequence of 3 values [1 2 3]?
; A: we use delimiters to break up a value; iff delims not present then all digits go into one value
; Problem: In ABNF, there are no ^ or $ values (beginning- and end-of-line).
; Solution: Since space is always a valid delimiter, add a single space go beginning
; and end of supplied source text, then parse
(dotest
  (let [abnf-src "
digits = 1*(%x30-39)  ; 0-9 / 1 or more
" ]
    (is= ((create-abnf-parser-raw abnf-src) "123")
      [:digits "1" "2" "3"] ))
  (let [abnf-src "
number = 1*digit
digit = %x30-39  ; 0-9
" ]
    (is= ((create-abnf-parser-raw abnf-src) "123")
      [:number
       [:digit "1"]
       [:digit "2"]
       [:digit "3"]]))
  (let [abnf-src "
file = *number
number = 1*digit
digit = %x30-39  ; 0-9
" ]
    (is= ((create-abnf-parser-raw abnf-src) "123")
      [:file [:number [:digit "1"]]
             [:number [:digit "2"]]
             [:number [:digit "3"]]] ))

  (let [abnf-src            "
digits        = ws 1*digit ws
digit         = %x30-39         ; 0-9
ws            = 1*' '           ; space: 1 or more
"
        parser-raw (create-abnf-parser-raw abnf-src) ]
    (throws? (parser-raw "123"))

    (is= (parser-raw (space-wrap "123"))
      [:digits [:ws " "] [:digit "1"] [:digit "2"] [:digit "3"] [:ws " "]])
    (is= (parser-raw (space-wrap " 123  "))
      [:digits [:ws " " " "] [:digit "1"] [:digit "2"] [:digit "3"] [:ws " " " " " "]]))
)

;-----------------------------------------------------------------------------
; Any token type (number, string, identifier, operator, etc) will wish to suppress leading/trailing
; whitespace. Write a convenience function to do that globally
(defn prune-whitespace-nodes [ast-in]
  (let [is-ws-child?      (fn [child] ; i.e. (is-ws-child? [:ws " " " "]) => true
                            (when (sequential? child)
                              (= :ws (first child))))
        prune-ws-children (fn [item]
                            (if (sequential? item)
                              (vec (remove is-ws-child? item))
                              item))
        ast-out           (walk/postwalk prune-ws-children ast-in)]
    ast-out))
(dotest
  (let [abnf-src            "
digits        = ws 1*digit ws
digit         = %x30-39         ; 0-9
ws            = 1*' '           ; space: 1 or more
"
        parser              (create-abnf-parser abnf-src)
        parse-and-transform (fn [src-text]
                              (let [parse-tree (parser src-text)
                                    ast-prune  (prune-whitespace-nodes parse-tree)]
                                ast-prune))
        ]
    (is= (parse-and-transform "123")
      [:digits [:digit "1"] [:digit "2"] [:digit "3"]])
    ))

;-----------------------------------------------------------------------------
; A sequence of digits is composed of multiple child :digit elementss. Join the child :digit elements.
(defn join-children-no-labels [children]
  (str/join
    (mapv second children)))
(dotest
  (let [abnf-src            "
digits        = ws 1*digit ws
digit         = %x30-39         ; 0-9
ws            = 1*' '           ; space: 1 or more
"
        tx-map              {
                             :digits (fn fn-digits [& args]
                                       [:digits (join-children-no-labels args)])
                             }
        parser              (create-abnf-parser abnf-src)
        parse-and-transform (fn [src-text]
                              (let [ast-parse (parser src-text)
                                    ast-prune  (prune-whitespace-nodes ast-parse)
                                    ast-tx    (insta/transform tx-map ast-prune) ]
                                ast-tx)) ]
    (is= (parse-and-transform "123") [:digits "123"])
    ))

;(defn sign? [item]
;  (and (sequential? item)
;    (= :sign (first item))))
;(defn signed? [item]
;  (and (sequential? item)
;    (= (sign? (first item)))))
;(is (sign? [:sign "+"]))
;(is (sign? [:sign "-"]))
;(is (sign? [:sign "xyz"]))
;(isnt (sign? [:foo "+"]))
;(isnt (sign? [:foo "xyz"]))
;(is (signed? [:xyz [:sign "+"] [:foo "xyz"]]))

;-----------------------------------------------------------------------------
; Define an integer as an optional +/- sign followed by a :digits element.
; After parsing, convert from a string to a integer value
(dotest
  (let [abnf-src            "
integer       = ws [ sign ] digits  ws  ; digits with optional sign
sign          = '+' / '-'       ; ignore + or - functions for now
digits        = 1*digit
digit         = %x30-39         ; 0-9
ws            = 1*' '           ; space: 1 or more
"
        tx-map              {
                             :digits  (fn fn-digits [& args] (join-children-no-labels args))
                             :sign    no-label
                             :integer (fn fn-integer [& args]
                                        (let [str-val (str/join args)
                                              result  (Integer/parseInt str-val)]
                                          [:integer result]))
                             }
        parser              (create-abnf-parser abnf-src)
        parse-and-transform (fn [src-text]
                              (let [ast-parse (parser src-text)
                                    ast-prune  (prune-whitespace-nodes ast-parse)
                                    ast-tx    (insta/transform tx-map ast-prune) ]
                                ast-tx))
        ]
    (is= (parse-and-transform "+123") [:integer +123])
    (is= (parse-and-transform "-123") [:integer -123])
    (is= (parse-and-transform "123") [:integer 123])
    ))

;-----------------------------------------------------------------------------
; An identifier is like an integer, except it must start with a letter or underscore. Following chars may
; be also include digits or hyphens.
(dotest
  (let [abnf-src            "
identifier              = ws identifier-start-char *identifier-body-char ws
identifier-start-char   = alpha / underscore
identifier-body-char    = alpha / underscore / digit / hyphen

integer                 = ws [ sign ] digits  ws  ; digits with optional sign

alpha                   = %x41-5A / %x61-7A     ; A-Z / a-z
hyphen                  = %x2D  ; - char
underscore              = %x5F  ; _ char
sign                    = '+' / '-'       ; ignore + or - functions for now
digits                  = 1*digit
digit                   = %x30-39         ; 0-9
ws                      = 1*' '           ; space: 1 or more
"
        tx-map              {
                             :digits     (fn fn-digits [& args] (join-children-no-labels args))
                             :sign       no-label
                             :integer    (fn fn-integer [& args]
                                           (let [str-val (str/join args)
                                                 result  (Integer/parseInt str-val)]
                                             [:integer result]))
                             :identifier (fn fn-identifier [& args]
                                           (let [v1 (mapv second args)
                                                 v2 (mapv second v1)
                                                 v3 (str/join v2)]
                                             [:identifier v3]))
                             }
        parser              (create-abnf-parser abnf-src)
        parse-and-transform (fn [src-text]
                              (let [ast-parse (parser src-text)
                                    ast-prune  (prune-whitespace-nodes ast-parse)
                                    ast-tx    (insta/transform tx-map ast-prune) ]
                                ast-tx))
        ]
    (is= (parse-and-transform "abc") [:identifier "abc"])
    ))

;-----------------------------------------------------------------------------
; Mix integers & identifiers together as "tokens". Note how we need to change the placement of whitespace
; to only the token/tokens rules (i.e. identifier & integer rules no longer have whitespace pieces). We
; have also moved (prune-whitespace-nodes ...) to the end of processing only.
(dotest
  (let [abnf-src            "
tokens                  = *token ws
token                   = ws (integer / identifier)
identifier              = identifier-start-char *identifier-body-char
identifier-start-char   = alpha / underscore
identifier-body-char    = alpha / underscore / digit / hyphen
integer                 = [ sign ] digits  ; digits with optional sign
alpha                   = %x41-5A / %x61-7A     ; A-Z / a-z
hyphen                  = %x2D  ; - char
underscore              = %x5F  ; _ char
sign                    = '+' / '-'       ; ignore + or - functions for now
digits                  = 1*digit
digit                   = %x30-39         ; 0-9
ws                      = 1*' '           ; space: 1 or more
"
        tx-map              {
                             :digits     (fn fn-digits [& args] (join-children-no-labels args))
                             :sign       no-label
                             :integer    (fn fn-integer [& args]
                                           (let [str-val (str/join args)
                                                 result  (Integer/parseInt str-val)]
                                             [:integer result]))
                             :identifier (fn fn-identifier [& args]
                                           (let [v1 (mapv second args) ; remove :identifier-start/body-char labels
                                                 v2 (mapv second v1) ; remove :alpha labels
                                                 v3 (str/join v2)] ; convert [ "a" "b" "c" ] -> "abc"
                                             [:identifier v3]))
                             }
        parser              (create-abnf-parser abnf-src)
        parse-and-transform (fn [src-text]
                              (let [ast-parse (parser src-text)
                                    ast-tx    (insta/transform tx-map ast-parse)
                                    ast-prune  (prune-whitespace-nodes ast-tx) ]
                                ast-prune))
        ]
    (is= (parse-and-transform "abc")  [:tokens [:token [:identifier "abc"]]])
    (is= (parse-and-transform "+123") [:tokens [:token [:integer +123]]])
    (is= (parse-and-transform "do-re-mi abc +123")
      [:tokens
       [:token [:identifier "do-re-mi"]]
       [:token [:identifier "abc"]]
       [:token [:integer +123]]])
    ))

;-----------------------------------------------------------------------------
; Mix integers & identifiers together as "tokens". Note how we need to change the placement of whitespace
; to only the token/tokens rules (i.e. identifier & integer rules no longer have whitespace pieces)
(defn quote-double? [item]
  (and (sequential? item)
    (= :quote-double (first item))))
(dotest
  (let [abnf-src            "
tokens                  = *token ws
token                   = ws (integer / identifier / string)
identifier              = identifier-start-char *identifier-body-char
integer                 = [ sign ] digits  ; digits with optional sign
string                  = quote-double *(ws / vis-char-no-dquote) quote-double   ; no escaping quotes yet

identifier-start-char   = alpha / underscore
identifier-body-char    = alpha / underscore / digit / hyphen
alpha                   = %x41-5A / %x61-7A     ; A-Z / a-z
hyphen                  = %x2D  ; - char
underscore              = %x5F  ; _ char
sign                    = '+' / '-'       ; ignore + or - functions for now
digits                  = 1*digit
digit                   = %x30-39         ; 0-9
ws                      = 1*' '           ; space: 1 or more
quote-double            = %x22
quote-single            = %x27
vis-char                = %x21-7E ; visible (printing) characters
vis-char-no-dquote      = %x21    / %x23-7E ; all visible chars without quote-double
"
        tx-map              {
                             :digits     (fn fn-digits [& args] (join-children-no-labels args))
                             :sign       no-label
                             :integer    (fn fn-integer [& args]
                                           (let [pruned  args
                                                 str-val (str/join pruned)
                                                 result  (Integer/parseInt str-val)]
                                             [:integer result]))
                             :identifier (fn fn-identifier [& args]
                                           (let [v1 (mapv second args) ; remove :identifier-start/body-char labels
                                                 v2 (mapv second v1) ; remove :alpha labels
                                                 v3 (str/join v2)] ; convert [ "a" "b" "c" ] -> "abc"
                                             [:identifier v3]))
                             :string     (fn fn-string [& args]
                                           (assert quote-double? (first args))
                                           (assert quote-double? (last args))
                                           (let [chars-keep (-> args rest butlast)
                                                 result     (join-children-no-labels chars-keep)]
                                             [:string result]))
                             }
        parser              (create-abnf-parser abnf-src)
        parse-and-transform (fn [src-text]
                              (let [ast-parse (parser src-text)
                                    ast-tx    (insta/transform tx-map ast-parse)
                                    ast-prune  (prune-whitespace-nodes ast-tx) ]
                                ast-prune))
        ]
    (is= (parse-and-transform (ts/quotes->double "'abc'"))
      [:tokens [:token [:string "abc"]]])

    ; All together now!
    (is=
      [:tokens
       [:token [:identifier "do-re-mi"]]
       [:token [:identifier "abc"]]
       [:token [:integer 1]]
       [:token [:integer 2]]
       [:token [:integer 3]]
       [:token [:identifier "baby"]]
       [:token [:string "you and me girl"]]]
      (parse-and-transform
           (ts/quotes->double "do-re-mi abc 1 2 3 baby 'you and me girl'")))
  ))

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
        tx-map              {
                             :digits     (fn fn-digits [& args] (str/join args))
                             :sign       no-label
                             :integer    (fn fn-integer [& args] [:integer (Integer/parseInt (str/join args))])
                             :identifier (fn fn-identifier [& args] [:identifier (str/join args)])
                             :string     (fn fn-string [& args] [:string (str/join args)])
                             }
        parser              (create-abnf-parser abnf-src )
        parse-and-transform (fn [src-text]
                              (let [ast-parse (parser src-text)
                                    ast-tx    (insta/transform tx-map ast-parse)
                                    _         (if (instaparse-failure? ast-tx)
                                                (throw (IllegalArgumentException. (str ast-tx)))
                                                ast-tx)
                                    ]
                                ast-tx))
        ]
    (is= (parse-and-transform "girl")
      [:tokens [:token [:identifier "girl"]]])
    (is= (parse-and-transform (ts/quotes->double "'abc'"))
      [:tokens [:token [:string "abc"]]])
    (is= (parse-and-transform +123)
      [:tokens [:token [:integer 123]]])
    (is= (parse-and-transform -123)
      [:tokens [:token [:integer -123]]])

    ; All together now!
    (is= (parse-and-transform
           (ts/quotes->double "do-re-mi abc 1 23 baby 'you and me girl'"))
      [:tokens
       [:token [:identifier "do-re-mi"]]
       [:token [:identifier "abc"]]
       [:token [:integer 1]]
       [:token [:integer 23]]
       [:token [:identifier "baby"]]
       [:token [:string "you and me girl"]]])))

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


        tx-map              {
                             :digits     (fn fn-digits [& args] (str/join args))
                             :sign       no-label
                             :integer    (fn fn-integer [& args] [:integer (Integer/parseInt (str/join args))])
                             :identifier (fn fn-identifier [& args] [:identifier (str/join args)])
                             :string     (fn fn-string [& args] [:string (str/join args)])
                            }
        parser              (create-abnf-parser abnf-src)
        parse-and-transform (fn [src-text]
                              (let [ast-parse (parser src-text)
                                    ast-tx    (insta/transform tx-map ast-parse)
                                    _         (if (instaparse-failure? ast-tx)
                                                (throw (IllegalArgumentException. (str ast-tx)))
                                                ast-tx)
                                    ]
                                ast-tx))
        ]
    (is= (parse-and-transform "girl.2")
      [:tokens [:token [:identifier "girl.2"]]])
    ))
