(ns tst.parse.calc0
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
    [tupelo.enlive :as te]
    [tupelo.gen :as tgen]
    [tupelo.misc :as tm]
    [tupelo.parse :as tp]
    [tupelo.schema :as tsk]
    [tupelo.string :as ts]
    [tupelo.x-forest :as tf]
    [tupelo.impl :as i])
  (:import [java.util.concurrent TimeoutException]
           [java.util List]))
(t/refer-tupelo)

(dotest
  (tf/with-forest (tf/new-forest)
    (let [abnf-src        (io/resource "yang3.abnf")
          yp              (create-abnf-parser abnf-src)
          yang-src        (slurp (io/resource "calc.yang"))

          yang-tree       (yp yang-src)
          yang-ast-hiccup (yang-transform yang-tree)
          yang-hid        (tf/add-tree-hiccup yang-ast-hiccup)
          yang-tree       (tf/hid->tree yang-hid)
          yang-ast-h2     (tf/tree->hiccup yang-tree)
          ]
      (is= yang-ast-hiccup yang-ast-h2
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

      (is= (tf/format-solns (tf/find-paths yang-hid [:module :rpc :identifier]))
        #{[{:tag :module}
           [{:tag :rpc}
            [{:tag :identifier} "add"]]]})

      (is= (tf/format-solns (tf/find-paths yang-hid [:module :revision]))
        #{[{:tag :module}
           [{:tag :revision}
            [{:tag :iso-date} "2017-04-01"]
            [{:tag :description} [{:tag :string} "Prototype 1.0"]]]]})

      (is= (tf/format-solns (tf/find-paths yang-hid [:module :rpc :input]))
        #{[{:tag :module}
           [{:tag :rpc}
            [{:tag :input}
             [{:tag :leaf}
              [{:tag :identifier} "x"]
              [{:tag :type} [{:tag :identifier} "decimal64"]]]
             [{:tag :leaf}
              [{:tag :identifier} "y"]
              [{:tag :type} [{:tag :identifier} "decimal64"]]]]]]})

      (is= (tf/format-solns (tf/find-paths yang-hid [:module :rpc :output]))
        #{[{:tag :module}
           [{:tag :rpc}
            [{:tag :output}
             [{:tag :leaf}
              [{:tag :identifier} "result"]
              [{:tag :type} [{:tag :identifier} "decimal64"]]]]]]})

      (let [solns (tf/find-paths yang-hid [:module :rpc :input :leaf])
            soln-elems (mapv last solns)]
        (is= (tf/format-solns solns)
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
                 :kids  [{:attrs {:tag :identifier}, :content ["y"]}
                         {:attrs {:tag :type},
                          :kids  [{:attrs {:tag :identifier}, :content ["decimal64"]}]}]}
                {:attrs {:tag :leaf},
                 :kids  [{:attrs {:tag :identifier}, :content ["x"]}
                         {:attrs {:tag :type},
                          :kids  [{:attrs {:tag :identifier}, :content ["decimal64"]}]}]}])) )
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
             [:identifier "decimal64"]]]])
        )
      )))

(dotest
  (let [rpc-call         [:rpc {:message-id 101 :xmlns "urn:ietf:params:xml:ns:netconf:base:1.0"}
                          [:add {:xmlns "my-own-ns/v1"}
                           [:x 2]
                           [:y 3]]]
        rpc-reply        [:rpc-reply {:message-id 101 :xmlns "urn:ietf:params:xml:ns:netconf:base:1.0"}
                          [:result 5]]
        rpc-call-enlive  (tf/hiccup->enlive rpc-call)
        rpc-reply-enlive (tf/hiccup->enlive rpc-reply)
        add-call         (grab :subtree (only (te/find-tree rpc-call-enlive [:rpc :add])))
        add-params       (grab :content add-call) ]
    (is= rpc-call-enlive
      {:tag   :rpc,
       :attrs {:message-id 101, :xmlns "urn:ietf:params:xml:ns:netconf:base:1.0"}
       :content
              [{:tag   :add,
                :attrs {:xmlns "my-own-ns/v1"},
                :content
                       [{:tag :x, :attrs {}, :content [2]}
                        {:tag :y, :attrs {}, :content [3]}]}]})
    (is= rpc-reply-enlive
      {:tag :rpc-reply, :attrs {:message-id 101, :xmlns "urn:ietf:params:xml:ns:netconf:base:1.0"}
       :content
            [{:tag :result, :attrs {}, :content [5]}]})
    (is= add-call
      {:tag     :add,
       :attrs   {:xmlns "my-own-ns/v1"},
       :content [{:tag :x, :attrs {}, :content [2]}
                 {:tag :y, :attrs {}, :content [3]}]})
    (is= add-params [{:tag :x, :attrs {}, :content [2]}
                     {:tag :y, :attrs {}, :content [3]}]) ))

;                               ---------type-------------------   -------pattern------- (or reverse)
; #todo need function (conforms [:type [:identifier "decimal64"]] [:type [:identifier :*]]
; #todo need function (conforms [:type [:identifier "decimal64"]] [:type [:identifier <string>]]

;-----------------------------------------------------------------------------

(def leaf-schema-1 (tf/hiccup->enlive [:leaf [:identifier "x"] [:type [:identifier "decimal64"]]]))
(def leaf-schema-2 (tf/hiccup->enlive [:leaf [:identifier "y"] [:type [:identifier "decimal64"]]]))
(def leaf-val-1 {:tag :x, :attrs {}, :content ["2"]})
(def leaf-val-2 {:tag :y, :attrs {}, :content ["3"]})
(def rpc-schema
  (tf/hiccup->enlive [:rpc
                   [:identifier "add"]
                   [:description [:string "Add 2 numbers"]]
                   [:input
                    [:leaf [:identifier "x"] [:type [:identifier "decimal64"]]]
                    [:leaf [:identifier "y"] [:type [:identifier "decimal64"]]]]
                   [:output
                    [:leaf [:identifier "result"] [:type [:identifier "decimal64"]]]]]))
(def rpc-input-val
  (tf/hiccup->enlive [:rpc {:message-id 101 :xmlns "urn:ietf:params:xml:ns:netconf:base:1.0"}
                   [:add {:xmlns "my-own-ns/v1"}
                    [:x "2"]
                    [:y "3"]]]))
(dotest
  (is= 2.0 (validate-parse-leaf leaf-schema-1 leaf-val-1))
  (is= 3.0 (validate-parse-leaf leaf-schema-2 leaf-val-2))
  (let [rpc-result (tf/enlive->hiccup (validate-parse-rpc-enlive rpc-schema rpc-input-val))]
    (is= rpc-result
      [:rpc-reply
       {:message-id 101, :xmlns "urn:ietf:params:xml:ns:netconf:base:1.0"}
       [:data 5.0]])))

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

  (let [abnf-src   (io/resource "yang3.abnf")
        yp         (create-abnf-parser abnf-src)
        yang-src   (slurp (io/resource "calc3.yang"))
        yang-tree  (yp yang-src)
        yang-ast-1 (yang-transform yang-tree)
        yang-ast-2 (tx-uses (tf/hiccup->enlive yang-ast-1))
        yang-ast-2-hiccup (tf/enlive->hiccup yang-ast-2)
        ]
    ;(spyx-pretty yang-ast-2-hiccup)
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
                             (let [filename-root (te/get-leaf subtree [:import :identifier])
                                   filename      (str filename-root ".yang")
                                   abnf-src      (io/resource "yang4.abnf")
                                   yp            (create-abnf-parser abnf-src)
                                   yang-src      (slurp (io/resource filename))
                                   yang-ast-0    (yp yang-src)
                                   yang-ast-1    (yang-transform yang-ast-0)
                                   yang-ast-1-i  (resolve-imports (tf/hiccup->enlive yang-ast-1))
                                   imp-typedefs  (forv [find-result (te/find-tree yang-ast-1-i [:module :typedef])]
                                                   (grab :subtree find-result))
                                   ]
                               imp-typedefs)))
            ast-resolved (update-in ast-no-import [:content] #(glue resolved-imports %))]
         ast-resolved))))

(dotest
  (let [abnf-src          (io/resource "yang4.abnf")
        yp                (create-abnf-parser abnf-src)
        yang-src          (slurp (io/resource "calc4.yang"))
        yang-ast-0        (yp yang-src)
        yang-ast-1        (yang-transform yang-ast-0)
        yang-ast-1-i      (resolve-imports
                            (tf/hiccup->enlive yang-ast-1))
        yang-ast-2        (tx-uses yang-ast-1-i)
        yang-ast-2-hiccup (tf/enlive->hiccup yang-ast-2) ]
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
        parser              (create-abnf-parser range-abnf)
        parse-and-transform (fn [src-text]
                              (let [ast-parse (parser src-text)
                                    ast-tx    (insta/transform tx-map ast-parse)]
                                ast-tx))]
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
                                          )) }
        parser              (create-abnf-parser range-abnf)
        parse-and-transform (fn [src-text]
                              (let [ast-parse (parser src-text)
                                    ast-tx    (insta/transform tx-map ast-parse)]
                                ast-tx))
        ]
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
  (let [iiinc-txt "(fn fn-iiinc [x] (+ 3 x))"
        iiinc-str-fn (eval (read-string iiinc-txt)) ]
    (is= 5 (iiinc-str-fn 2)))

  (let [iiinc-ast '(fn fn-iiinc [x] (+ 3 x))
        iiinc-ast-fn (eval iiinc-ast) ]
    (is= 5 (iiinc-ast-fn 2)))

  (let [state (atom {})
        yang-forest
          (tf/with-forest (tf/new-forest)
            (let [abnf-src        (io/resource "yang3.abnf")
                  yp              (create-abnf-parser abnf-src)
                  yang-src        (slurp (io/resource "calc.yang"))

                  yang-tree       (yp yang-src)
                  yang-ast-hiccup (yang-transform yang-tree)
                  yang-hid        (tf/add-tree-hiccup yang-ast-hiccup)
                  yang-tree       (tf/hid->tree yang-hid)
                  yang-ast-h2     (tf/tree->hiccup yang-tree)
                  ]
              (reset! state (vals->map yang-hid))
              (is= yang-ast-hiccup yang-ast-h2
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

              (is= (tf/format-solns (tf/find-paths yang-hid [:module :rpc :identifier]))
                #{[{:tag :module}
                   [{:tag :rpc}
                    [{:tag :identifier} "add"]]]}) ))]

    (tf/with-forest yang-forest
      (with-map-vals @state [yang-hid]
        (let [rpc-hid (tf/find-hid yang-hid [:module :rpc])]
          (let [rpc-hiccup (tf/hid->hiccup rpc-hid)
                rpc-bush   (tf/hid->bush rpc-hid)
                rpc-tree   (tf/hid->tree rpc-hid) ]
            (is= rpc-hiccup
              [:rpc
               [:identifier "add"]
               [:description [:string "Add 2 numbers"]]
               [:input
                [:leaf [:identifier "x"] [:type [:identifier "decimal64"]]]
                [:leaf [:identifier "y"] [:type [:identifier "decimal64"]]]]
               [:output
                [:leaf [:identifier "result"] [:type [:identifier "decimal64"]]]]])
           ;(spyx-pretty rpc-tree )
           ;(spyx-pretty rpc-bush )
          )

          (tx-rpc rpc-hid)
          (is= (tf/hid->bush rpc-hid)
            [{:tag :rpc, :name :add}
             [{:tag :input}
              [{:tag :leaf, :type :decimal64, :name :x}]
              [{:tag :leaf, :type :decimal64, :name :y}]]
             [{:tag :output} [{:tag :leaf, :type :decimal64, :name :result}]]])

          (is= (tf/hid->hiccup rpc-hid)
            [:rpc {:name :add}
             [:input
              [:leaf {:type :decimal64, :name :x}]
              [:leaf {:type :decimal64, :name :y}]]
             [:output [:leaf {:type :decimal64, :name :result}]]])

          (is= (rpc->api rpc-hid)
            '(fn fn-add [x y] (fn-add-impl x y)))

      )))))
