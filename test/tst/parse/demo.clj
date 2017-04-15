(ns tst.parse.demo
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
    (is= [:string "hello"] s1-ast))
  (let [abnf-src (str abnf-identifier abnf-base)
        yp       (create-abnf-parser abnf-src)
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
  (let [abnf-src (str abnf-tokens abnf-string abnf-identifier abnf-base)
        yp          (create-abnf-parser abnf-src)
        s1          (ts/quotes->double "  ident1 ")
        s1-tree     (yp s1)
        s1-ast      (yang-transform s1-tree)
        s2          (ts/quotes->double "  ident1 'str1' ident2 'str2' ")
        s2-tree     (yp s2)
        s2-ast      (yang-transform s2-tree) ]
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
                             [:token [:string     "str2"]]] )))))))

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
       [:prefix [:identifier "toast"]]
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
        ]] )))

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
        [:rpc-input
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
        [:description [:string "Stop making toast, if any is being made."]]]] )))

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
    (println (pretty-str yang-ast))
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
      ] )))


(dotest
  (let [abnf-src            "
size-val      = int / int-px
int           = digits          ; ex '123'
int-px        = digits <'px'>   ; ex '123px'
<digits>      = 1*digit         ; 1 or more digits
<digit>       = %x30-39         ; 0-9
"
        tx-map              {:int      (fn fn-int [& args]
                                         [:int (Integer/parseInt (str/join args))])
                             :int-px   (fn fn-int-px [& args]
                                         [:int-px (Integer/parseInt (str/join args))])
                             :size-val identity
                             }

        parser              (insta/parser abnf-src :input-format :abnf)
        instaparse-failure? (fn [arg] (= (class arg) instaparse.gll.Failure))
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
