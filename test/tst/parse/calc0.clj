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
    [tupelo.gen :as tgen]
    [tupelo.misc :as tm]
    [tupelo.parse :as tp]
    [tupelo.schema :as tsk]
    [tupelo.string :as ts]
  )
  (:import [java.util.concurrent TimeoutException]
           [java.util List]))
(t/refer-tupelo)

(dotest
  (let [abnf-src  (io/resource "yang3.abnf")
        yp        (create-abnf-parser abnf-src)
        yang-src  (slurp (io/resource "calc.yang"))

        yang-tree (yp yang-src)
        yang-ast  (yang-transform yang-tree) ]
    (is= yang-ast
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
         [:leaf [:identifier "result"] [:type [:identifier "decimal64"]]]]]] )

    (is= (find-tree-hiccup yang-ast [:module :rpc :identifier])
      #{ {:parent-path [:module :rpc], :subtree [:identifier "add"]} } )
    (is= (find-tree-hiccup yang-ast [:module :revision])
      #{{:parent-path [:module]
         :subtree     [:revision
                       [:iso-date "2017-04-01"]
                       [:description [:string "Prototype 1.0"]]]}})
    (is= (find-tree-hiccup yang-ast [:module :rpc :input])
      #{ {:parent-path [:module :rpc],
          :subtree     [:input
                        [:leaf [:identifier "x"] [:type [:identifier "decimal64"]]]
                        [:leaf [:identifier "y"] [:type [:identifier "decimal64"]]]]}})
    (is= (find-tree-hiccup yang-ast [:module :rpc :output])
      #{{:parent-path [:module :rpc],
         :subtree     [:output
                       [:leaf
                        [:identifier "result"]
                        [:type [:identifier "decimal64"]]]]}} )
    (is= (find-tree-hiccup yang-ast [:module :rpc :input :leaf])
      #{{:parent-path [:module :rpc :input],
         :subtree [:leaf [:identifier "x"] [:type [:identifier "decimal64"]]]}
        {:parent-path [:module :rpc :input],
         :subtree [:leaf [:identifier "y"] [:type [:identifier "decimal64"]]]} } )
  ))

(dotest
  (let [rpc-call         [:rpc {:message-id 101 :xmlns "urn:ietf:params:xml:ns:netconf:base:1.0"}
                          [:add {:xmlns "my-own-ns/v1"}
                           [:x 2]
                           [:y 3]]]
        rpc-reply        [:rpc-reply {:message-id 101 :xmlns "urn:ietf:params:xml:ns:netconf:base:1.0"}
                          [:result 5]]
        rpc-call-enlive  (hiccup->enlive rpc-call)
        rpc-reply-enlive (hiccup->enlive rpc-reply)
        add-call         (grab :subtree (only (find-tree rpc-call-enlive [:rpc :add])))
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

(def leaf-schema-1 (hiccup->enlive [:leaf [:identifier "x"] [:type [:identifier "decimal64"]]]))
(def leaf-schema-2 (hiccup->enlive [:leaf [:identifier "y"] [:type [:identifier "decimal64"]]]))
(def leaf-val-1 {:tag :x, :attrs {}, :content ["2"]})
(def leaf-val-2 {:tag :y, :attrs {}, :content ["3"]})
(def rpc-schema
  (hiccup->enlive [:rpc
                   [:identifier "add"]
                   [:description [:string "Add 2 numbers"]]
                   [:input
                    [:leaf [:identifier "x"] [:type [:identifier "decimal64"]]]
                    [:leaf [:identifier "y"] [:type [:identifier "decimal64"]]]]
                   [:output
                    [:leaf [:identifier "result"] [:type [:identifier "decimal64"]]]]]))
(def rpc-input-val
  (hiccup->enlive [:rpc {:message-id 101 :xmlns "urn:ietf:params:xml:ns:netconf:base:1.0"}
                   [:add {:xmlns "my-own-ns/v1"}
                    [:x "2"]
                    [:y "3"]]]))
(dotest
  (is= 2.0 (validate-parse-leaf leaf-schema-1 leaf-val-1))
  (is= 3.0 (validate-parse-leaf leaf-schema-2 leaf-val-2))
  (let [rpc-result (enlive->hiccup (validate-parse-rpc rpc-schema (spyx-pretty rpc-input-val)))]
    (is= (spyx rpc-result)
      [:rpc-reply
       {:message-id 101, :xmlns "urn:ietf:params:xml:ns:netconf:base:1.0"}
       [:data 5.0]])))

;-----------------------------------------------------------------------------
(dotest
  (is= (container-with-uses? (hiccup->enlive
                               [:container
                                [:identifier "result"]
                                [:uses [:identifier "complex-grp"]]] )))
  (is= (container-with-uses? (hiccup->enlive
                               [:container
                                [:identifier "result"]
                                [:leaf [:identifier "y"] [:type [:identifier "decimal64"]]]] )))

  (is= (uses-replace
         {"complex-grp" (hiccup->enlive [:stuff [:a 1] [:b 2]])}
         (hiccup->enlive
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
        yang-ast-2 (tx-uses yang-ast-1)
        yang-ast-2-hiccup (enlive->hiccup yang-ast-2)
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
          ]]]] )
  )
)
