(ns tst.parse.calc0
  (:use parse.core
        parse.transform
        tupelo.test )
  (:require
    [clojure.java.io :as io]
    [clojure.string :as str]
    [schema.core :as s]
    [tupelo.core :as t]
    [tupelo.enlive :as te]
    [tupelo.schema :as tsk]
    [tupelo.x-forest :as tf]
  ))
(t/refer-tupelo)

(dotest
  (tf/with-forest (tf/new-forest)
    (let [abnf-src            (io/resource "yang3.abnf")
          yang-src            (slurp (io/resource "calc.yang"))
          parse-and-transform (create-parser-transformer abnf-src yang-tx-map)
          yang-ast-hiccup     (parse-and-transform yang-src)
          yang-hid            (tf/add-tree-hiccup yang-ast-hiccup)
          yang-tree           (tf/hid->tree yang-hid) ]
      (is= yang-ast-hiccup
        [:module
         [:identifier "calculator"]
         [:namespace [:string "http://brocade.com/ns/calculator"]]
         [:contact [:string "Alan Thompson <athomps@brocade.com>"]]
         [:description [:string "YANG spec for a simple RPN calculator"]]
         [:revision
          [:iso-date "2017-04-01"]
          [:description [:string "Prototype 1.0"]]]
         [:rpc
          [:identifier "add"]
          [:description [:string "Add 2 numbers"]]
          [:input
           [:leaf [:identifier "x"] [:type [:identifier "decimal64"]]]
           [:leaf [:identifier "y"] [:type [:identifier "decimal64"]]]]
          [:output
           [:leaf [:identifier "result"] [:type [:identifier "decimal64"]]]]]])

      (is= (tf/format-paths (tf/find-paths yang-hid [:module :rpc :identifier]))
        #{[{:tag :module}
           [{:tag :rpc}
            [{:tag :identifier} "add"]]]})

      (is= (tf/format-paths (tf/find-paths yang-hid [:module :revision]))
        #{[{:tag :module}
           [{:tag :revision}
            [{:tag :iso-date} "2017-04-01"]
            [{:tag :description} [{:tag :string} "Prototype 1.0"]]]]})

      (is= (tf/format-paths (tf/find-paths yang-hid [:module :rpc :input]))
        #{[{:tag :module}
           [{:tag :rpc}
            [{:tag :input}
             [{:tag :leaf}
              [{:tag :identifier} "x"]
              [{:tag :type} [{:tag :identifier} "decimal64"]]]
             [{:tag :leaf}
              [{:tag :identifier} "y"]
              [{:tag :type} [{:tag :identifier} "decimal64"]]]]]]})

      (is= (tf/format-paths (tf/find-paths yang-hid [:module :rpc :output]))
        #{[{:tag :module}
           [{:tag :rpc}
            [{:tag :output}
             [{:tag :leaf}
              [{:tag :identifier} "result"]
              [{:tag :type} [{:tag :identifier} "decimal64"]]]]]]})

      (let [solns (tf/find-paths yang-hid [:module :rpc :input :leaf])
            soln-elems (mapv last solns)]
        (is= (tf/format-paths solns)
          #{[{:tag :module}
             [{:tag :rpc}
              [{:tag :input}
               [{:tag :leaf}
                [{:tag :identifier} "x"]
                [{:tag :type} [{:tag :identifier} "decimal64"]]]]]]
            [{:tag :module}
             [{:tag :rpc}
              [{:tag :input}
               [{:tag :leaf}
                [{:tag :identifier} "y"]
                [{:tag :type} [{:tag :identifier} "decimal64"]]]]]]})
        (is= (set (forv [elem soln-elems]
                    (tf/hid->tree elem)))
          (set [{:attrs {:tag :leaf},
                 :kids  [{:attrs {:tag :identifier}, :value "y"}
                         {:attrs {:tag :type},
                          :kids  [{:attrs {:tag :identifier}, :value "decimal64"}]}]}
                {:attrs {:tag :leaf},
                 :kids  [{:attrs {:tag :identifier}, :value "x"}
                         {:attrs {:tag :type},
                          :kids  [{:attrs {:tag :identifier}, :value "decimal64"}]}]}])) )

      (let [soln-hid (last (only (tf/find-paths yang-hid [:module :rpc :input])))]
        (is= (tf/hid->bush soln-hid)
          [{:tag :input}
           [{:tag :leaf}
            [{:tag :identifier} "x"]
            [{:tag :type}
             [{:tag :identifier} "decimal64"]]]
           [{:tag :leaf}
            [{:tag :identifier} "y"]
            [{:tag :type}
             [{:tag :identifier} "decimal64"]]]])
        (is= (tf/hid->hiccup soln-hid)
          [:input
           [:leaf
            [:identifier "x"]
            [:type
             [:identifier "decimal64"]]]
           [:leaf
            [:identifier "y"]
            [:type
             [:identifier "decimal64"]]]]) ) )))

(dotest
  (tf/with-forest (tf/new-forest)
    (let [rpc-call-hid (tf/add-tree-hiccup
                         [:rpc {:message-id 101 :xmlns "urn:ietf:params:xml:ns:netconf:base:1.0"}
                          [:add
                           [:x 2]
                           [:y 3]]])

          rpc-reply-hid (tf/add-tree-hiccup
                          [:rpc-reply {:message-id 101 :xmlns "urn:ietf:params:xml:ns:netconf:base:1.0"}
                           [:result 5]])

          add-hid (tf/find-hid rpc-call-hid [:rpc :add])
          add-kids (grab :kids (tf/hid->node add-hid))]
      (is= (tf/hid->hiccup add-hid)
        [:add [:x 2] [:y 3]])
      (is (submatch? (mapv tf/hid->leaf add-kids)
            [{:attrs {:tag :x}, :value 2}
             {:attrs {:tag :y}, :value 3}])))))

;                               ---------type-------------------   -------pattern------- (or reverse)
; #todo need function (conforms [:type [:identifier "decimal64"]] [:type [:identifier :*]]
; #todo need function (conforms [:type [:identifier "decimal64"]] [:type [:identifier <string>]]

;-----------------------------------------------------------------------------
(dotest
  (is= (container-with-uses? (tf/hiccup->enlive
                               [:container
                                [:identifier "result"]
                                [:uses [:identifier "complex-grp"]]] )))
  (is= (container-with-uses? (tf/hiccup->enlive
                               [:container
                                [:identifier "result"]
                                [:leaf [:identifier "y"] [:type [:identifier "decimal64"]]]] )))

  (is= (uses-replace
         {"complex-grp" (tf/hiccup->enlive [:stuff [:a 1] [:b 2]])}
         (tf/hiccup->enlive
           [:container
            [:identifier "result"]
            [:uses [:identifier "complex-grp"]]]))
    {:tag     :container,
     :attrs   {},
     :content [{:tag :identifier, :attrs {}, :content ["result"]}
               {:tag :a, :attrs {}, :content [1]}
               {:tag :b, :attrs {}, :content [2]}]})

  (let [abnf-src            (io/resource "yang3.abnf")
        yang-src            (slurp (io/resource "calc3.yang"))
        parse-and-transform (create-parser-transformer abnf-src yang-tx-map)
        yang-ast-1          (parse-and-transform yang-src)
        yang-ast-2          (tx-uses (tf/hiccup->enlive yang-ast-1))
        yang-ast-2-hiccup   (tf/enlive->hiccup yang-ast-2) ]
    (is= yang-ast-2-hiccup
      [:module
       [:identifier "calculator"]
       [:namespace [:string "http://brocade.com/ns/calculator"]]
       [:contact [:string "Alan Thompson <athomps@brocade.com>"]]
       [:description [:string "YANG spec for a simple RPN calculator"]]
       [:revision
        [:iso-date "2017-04-01"]
        [:description [:string "Prototype 1.0"]]]
       [:grouping
        [:identifier "complex-grp"]
        [:description [:string "A complex number"]]
        [:leaf [:identifier "real"] [:type [:identifier "decimal64"]]]
        [:leaf [:identifier "imag"] [:type [:identifier "decimal64"]]]]

       [:rpc
        [:identifier "add"]
        [:description [:string "Add 2 numbers"]]
        [:input
         [:leaf [:identifier "x"] [:type [:identifier "decimal64"]]]
         [:leaf [:identifier "y"] [:type [:identifier "decimal64"]]]]
        [:output
         [:leaf [:identifier "result"] [:type [:identifier "decimal64"]]]]]

       [:rpc
        [:identifier "addc"]
        [:description [:string "Add 2 numbers"]]
        [:input
         [:container [:identifier "x"]
          [:leaf [:identifier "real"] [:type [:identifier "decimal64"]]]
          [:leaf [:identifier "imag"] [:type [:identifier "decimal64"]]]]
         [:container [:identifier "y"]
          [:leaf [:identifier "real"] [:type [:identifier "decimal64"]]]
          [:leaf [:identifier "imag"] [:type [:identifier "decimal64"]]]]]
        [:output
         [:container [:identifier "result"]
          [:leaf [:identifier "real"] [:type [:identifier "decimal64"]]]
          [:leaf [:identifier "imag"] [:type [:identifier "decimal64"]]]
          ]]]] ) ) )

; #todo need to test recursive imports
(defn import? [arg] (= :import (grab :tag arg)))

(defn resolve-imports [ast]
  ; imports must be at the top level
  (let [children               (grab :content ast)
        ast-children-import    (keep-if import? children)
        ast-children-no-import (drop-if import? children)
        ast-no-import          (assoc ast :content ast-children-no-import)
        ]
    (if (empty? ast-children-import)
      ast
      (let [resolved-imports
              (apply glue
                (forv [subtree ast-children-import]
                  (let [filename-root       (te/get-leaf subtree [:import :identifier])
                        filename            (str filename-root ".yang")
                        abnf-src            (io/resource "yang4.abnf")
                        yang-src            (slurp (io/resource filename))
                        parse-and-transform (create-parser-transformer abnf-src yang-tx-map)
                        yang-ast-1          (parse-and-transform yang-src)
                        yang-ast-1-i        (resolve-imports (tf/hiccup->enlive yang-ast-1))
                        imp-typedefs        (forv [find-result (te/find-tree yang-ast-1-i [:module :typedef])]
                                              (grab :subtree find-result)) ]
                    imp-typedefs)))
            ast-resolved
              (update-in ast-no-import [:content] #(glue resolved-imports %))]
        ast-resolved))))

(dotest
  (let [abnf-src            (io/resource "yang4.abnf")
        yang-src            (slurp (io/resource "calc4.yang"))
        parse-and-transform (create-parser-transformer abnf-src yang-tx-map)
        yang-ast-1          (parse-and-transform yang-src)
        yang-ast-1-i        (resolve-imports (tf/hiccup->enlive yang-ast-1))
        yang-ast-2          (tx-uses yang-ast-1-i)
        yang-ast-2-hiccup   (tf/enlive->hiccup yang-ast-2)]
    (is= yang-ast-2-hiccup
      [:module
       [:typedef
        [:identifier "octal-digit"]
        [:description [:string "An octal digit [0..7]"]]
        [:type [:identifier "uint32"]]]
       [:identifier "calculator"]
       [:namespace [:string "http://brocade.com/ns/calculator"]]
       [:contact [:string "Alan Thompson <athomps@brocade.com>"]]
       [:description [:string "YANG spec for a simple RPN calculator"]]
       [:revision
        [:iso-date "2017-04-01"]
        [:description [:string "Prototype 1.0"]]]
       [:rpc
        [:identifier "add"]
        [:description [:string "Add 2 numbers"]]
        [:input
         [:leaf [:identifier "x"] [:type [:identifier "decimal64"]]]
         [:leaf [:identifier "y"] [:type [:identifier "decimal64"]]]]
        [:output
         [:leaf [:identifier "result"] [:type [:identifier "decimal64"]]]]]
       ])))

(def range-abnf "
range                   = <ws> integer <ows> <dotdot> <ows> integer <ws>
integer                 = 0*1sign digits  ; digits with optional sign
<dotdot>                = '..'
<sign>                  = '+' / '-'       ; ignore + or - functions for now
digits                  = 1*digit
<digit>                 = %x30-39         ; 0-9
<ws>                    = 1*' '           ; whitespace:          1 or more
<ows>                   =  *' '           ; optional whitespace: 0 or more
")

(dotest
  (let [tx-map              {:digits  (fn fn-digits [& args] (str/join args))
                             :integer (fn fn-integer [& args] [:integer (Integer/parseInt (str/join args))])
                             }
        parse-and-transform (create-parser-transformer range-abnf tx-map)]
    (is= [:range [:integer 123] [:integer 456]] (parse-and-transform "123..456"))
    (is= [:range [:integer 123] [:integer 456]] (parse-and-transform "123 .. 456"))
    (is= [:range [:integer 123] [:integer 456]] (parse-and-transform " 123 .. 456  ")))

  (let [tx-map              {:digits  (fn fn-digits [& args] (str/join args))
                             :integer (fn fn-integer [& args] [:integer (Integer/parseInt (str/join args))])
                             :range   (fn fn-range [& args]
                                        ; Input is like:  [:range [:integer 123] [:integer 456]]
                                        (let [low  (-> args first second)
                                              high (-> args second second)]
                                          (assert (<= low high))
                                          [:range {:low         low
                                                   :high        high
                                                   :fn-validate (fn [arg] (<= low arg high))}]
                                          ))}
        parse-and-transform (create-parser-transformer range-abnf tx-map)]
    (is (wild-match? [:range {:low 123 :high 456 :fn-validate :*}]
          (parse-and-transform "123..456")
          (parse-and-transform "123 .. 456")
          (parse-and-transform "  123 .. 456 ")))
    (is (wild-match? [:range {:low -123 :high -45 :fn-validate :*}] (parse-and-transform "-123..-45")))
    (let [parse-result (parse-and-transform "123..456")
          fn-validate  (grab :fn-validate (second parse-result))]
      (is (truthy? (fn-validate 234)))
      (is (falsey? (fn-validate 111)))
      (is (falsey? (fn-validate 999))))))

;-----------------------------------------------------------------------------

(dotest
  (tf/with-forest (tf/new-forest)
    (let [abnf-src            (io/resource "yang3.abnf")
          yang-src            (slurp (io/resource "calc.yang"))
          parse-and-transform (create-parser-transformer abnf-src yang-tx-map)
          yang-ast-hiccup     (parse-and-transform yang-src)
          yang-hid            (tf/add-tree-hiccup yang-ast-hiccup) ]
      (is= yang-ast-hiccup (tf/hid->hiccup yang-hid)
        [:module
         [:identifier "calculator"]
         [:namespace [:string "http://brocade.com/ns/calculator"]]
         [:contact [:string "Alan Thompson <athomps@brocade.com>"]]
         [:description [:string "YANG spec for a simple RPN calculator"]]
         [:revision
          [:iso-date "2017-04-01"]
          [:description [:string "Prototype 1.0"]]]
         [:rpc
          [:identifier "add"]
          [:description [:string "Add 2 numbers"]]
          [:input
           [:leaf [:identifier "x"] [:type [:identifier "decimal64"]]]
           [:leaf [:identifier "y"] [:type [:identifier "decimal64"]]]]
          [:output
           [:leaf [:identifier "result"] [:type [:identifier "decimal64"]]]]]])

      (is= (tf/format-paths (tf/find-paths yang-hid [:module :rpc :identifier]))
        #{[{:tag :module}
           [{:tag :rpc}
            [{:tag :identifier} "add"]]]})

      (let [rpc-hid (tf/find-hid yang-hid [:module :rpc])]
        (is= (tf/hid->hiccup rpc-hid)
          [:rpc
           [:identifier "add"]
           [:description [:string "Add 2 numbers"]]
           [:input
            [:leaf [:identifier "x"] [:type [:identifier "decimal64"]]]
            [:leaf [:identifier "y"] [:type [:identifier "decimal64"]]]]
           [:output
            [:leaf [:identifier "result"] [:type [:identifier "decimal64"]]]]])

        (tx-rpc rpc-hid)
        (is= (tf/hid->bush rpc-hid)
          [{:tag :rpc, :name :add}
           [{:tag :input}
            [{:type :decimal64, :name :x}]
            [{:type :decimal64, :name :y}]]
           [{:tag :output}
            [{:type :decimal64, :name :result}]]])

        (is= (rpc->api rpc-hid)
          '(fn fn-add [x y] (fn-add-impl x y)))))))

