(ns tst.parse.calc2
  (:use parse.core
        parse.transform
        tupelo.test
        clojure.test)
  (:require
    [clojure.core.async :as async]
    [clojure.java.io :as io]
    [clojure.string :as str]
    [schema.core :as s]
    [tupelo.core :as t]
    [tupelo.misc :as tm]
    [tupelo.schema :as tsk]
    [tupelo.string :as ts]
    [tupelo.x-forest :as tf]
  ))
(t/refer-tupelo)

(def ^:dynamic *rpc-timeout-ms* 200)
(def ^:dynamic *rpc-delay-simulated-ms* 30)

(dotest
  (tf/with-forest (tf/new-forest)
    (let [abnf-src            (io/resource "yang3.abnf")
          yang-src            (slurp (io/resource "calc.yang"))
          parse-and-transform (create-parser-transformer abnf-src yang-tx-map)
          yang-ast-hiccup     (parse-and-transform yang-src)
          yang-hid            (tf/add-tree-hiccup yang-ast-hiccup)
          rpc-hid             (tf/find-hid yang-hid [:module :rpc])]
      (tx-rpc rpc-hid)
      (is= (tf/hid->bush rpc-hid)
        [{:tag :rpc, :name :add}
         [{:tag :input}
          [{:tag :leaf, :type :decimal64, :name :x}]
          [{:tag :leaf, :type :decimal64, :name :y}]]
         [{:tag :output} [{:tag :leaf, :type :decimal64, :name :result}]]])

      (is= (rpc->api rpc-hid)
        '(fn fn-add [x y] (fn-add-impl x y)))
      (is= (rpc-marshall rpc-hid [2 3])
        [:rpc [:add {:xmlns "my-own-ns/v1"} [:x "2"] [:y "3"]]])
)))

;-----------------------------------------------------------------------------
(def rpc-msg-id (atom 100))
(def rpc-msg-id-map (atom {}))

(defn add [x y]
  (tf/with-forest (tf/new-forest)
    ; #todo  keywordize all :identifier values
    ; #todo  need to massage schema like below
    (let [rpc-schema-hid (tf/add-tree-hiccup
                           [:rpc
                            [:identifier :add]
                            [:description [:string "Add 2 numbers"]]
                            [:input
                             [:x {:type :decimal64}] ; #todo :identifier -> :tag
                             [:y {:type :decimal64}] ]
                            [:output
                             [:leaf {:identifier :result :type :decimal64} ]]])

          rpc-hid        (tf/add-tree-hiccup
                           [:rpc
                            [:add {:xmlns "my-own-ns/v1"}
                             [:x (str x)]
                             [:y (str y)]]])

          result-promise (promise)
          rpc-msg-id     (swap! rpc-msg-id inc)
          xx             (swap! rpc-msg-id-map glue {rpc-msg-id result-promise})
          xx             (tf/update-attrs rpc-hid
                           (fn [attrs] (glue attrs {:message-id rpc-msg-id
                                                    :xmlns      "urn:ietf:params:xml:ns:netconf:base:1.0"})))
          result-hid     (validate-parse-rpc-tree rpc-schema-hid rpc-hid)
              ;(tf/hid->tree result-hid)
              ;    {:attrs {:tag :rpc-reply, :message-id 101, :xmlns "urn:ietf:params:xml:ns:netconf:base:1.0"},
              ;     :kids  [{:attrs {:tag :data}, :content [5.0]}]}
          result-value   (only (tf/find-leaf-content result-hid [:rpc-reply :data]))
          ]
      ; result-hid (deref result-promise *rpc-timeout-ms* ::timeout-failure)
      ;(when (instance? Throwable result-hid)
      ;  (throw (RuntimeException. (.getMessage result-hid))))
      ;(when (= ::timeout-failure result)
      ;  (throw (TimeoutException. (format "Timeout Exceed=%s  add: %s %s; " *rpc-timeout-ms* x y))))
      result-value )))

(dotest
  (binding [*rpc-timeout-ms*        100
            *rpc-delay-simulated-ms* 10]
    (reset! rpc-msg-id 100)
    (is (rel= 5 (add 2 3) :digits 9)) ))


