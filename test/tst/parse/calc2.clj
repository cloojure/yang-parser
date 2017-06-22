(ns tst.parse.calc2
  (:use parse.core
        parse.transform
        tupelo.test)
  (:require
    [clojure.java.io :as io]
    [tupelo.core :as t]
    [tupelo.forest :as tf] ))
(t/refer-tupelo)

(def ^:dynamic *rpc-timeout-ms* 100)
(def ^:dynamic *rpc-delay-simulated-ms* 10)

;-----------------------------------------------------------------------------
(dotest
  (let [abnf-src (io/resource "yang3.abnf")
        yang-src (slurp (io/resource "calc.yang"))
        parse-and-transform (create-parser-transformer abnf-src yang-tx-map)
        yang-ast-hiccup (parse-and-transform yang-src)]

    (tf/with-forest (tf/new-forest)
      (let [module-hid    (tf/add-tree-hiccup yang-ast-hiccup)
            rpc-hid       (tf/find-hid module-hid [:module :rpc])

            leaf-hids     (tf/find-hids rpc-hid [:rpc :* :leaf])
            leaves-before (forv [leaf-hid leaf-hids]
                            (tf/hid->hiccup leaf-hid))
            >>            (doseq [leaf-hid leaf-hids]
                            (leaf-name->attrs leaf-hid)
                            (leaf-type->attrs leaf-hid))
            leaves-after  (forv [leaf-hid leaf-hids]
                            (tf/hid->hiccup leaf-hid))]
        (is= leaves-before
          [[:leaf [:identifier "x"] [:type [:identifier "decimal64"]]]
           [:leaf [:identifier "y"] [:type [:identifier "decimal64"]]]
           [:leaf [:identifier "result"] [:type [:identifier "decimal64"]]]])
        (is= leaves-after
          [[:leaf {:name :x, :type :decimal64}]
           [:leaf {:name :y, :type :decimal64}]
           [:leaf {:name :result, :type :decimal64}]])))

    (tf/with-forest (tf/new-forest)
      (reset-rpc-msg-id 100)
      (let [module-hid         (tf/add-tree-hiccup yang-ast-hiccup)
            schema-hid         (tf/find-hid module-hid [:module :rpc])
            schema-bush-before (tf/hid->bush schema-hid)
            >>                 (tx-rpc schema-hid)
            schema-bush-after  (tf/hid->bush schema-hid)
            rpc-api-clj        (rpc->api schema-hid)
            call-msg           (rpc-call-marshall schema-hid [2 3])
            msg-marshalled-hid (tf/add-tree-hiccup call-msg)
            call-unmarshalled  (rpc-call-unmarshall schema-hid msg-marshalled-hid)
            call-result        (invoke-rpc call-unmarshalled)
            reply-msg          (rpc-reply-marshall schema-hid msg-marshalled-hid call-result)
            reply-hid          (tf/add-tree-hiccup reply-msg)
            reply-val          (reply-unmarshall schema-hid reply-hid)]
        (is= schema-bush-before
          [{:tag :rpc}
           [{:tag :identifier} "add"]
           [{:tag :description} [{:tag :string} "Add 2 numbers"]]
           [{:tag :input}
            [{:tag :leaf}
             [{:tag :identifier} "x"]
             [{:tag :type} [{:tag :identifier} "decimal64"]]]
            [{:tag :leaf}
             [{:tag :identifier} "y"]
             [{:tag :type} [{:tag :identifier} "decimal64"]]]]
           [{:tag :output}
            [{:tag :leaf}
             [{:tag :identifier} "result"]
             [{:tag :type} [{:tag :identifier} "decimal64"]]]]])
        (is= schema-bush-after
          [{:tag :rpc, :name :add}
           [{:tag :input}
            [{:type :decimal64, :name :x}]
            [{:type :decimal64, :name :y}]]
           [{:tag :output} [{:type :decimal64, :name :result}]]])
        (is= rpc-api-clj '(fn fn-add [x y] (fn-add-impl x y)))
        (is= call-msg [:rpc [:add {:message-id 101}
                             [:x "2"] [:y "3"]]])
        (is (wild-match? {:rpc-fn :*, :args [2.0 3.0]} call-unmarshalled))
        (is= call-result 5.0)
        (is= reply-msg
          [:rpc-reply {:message-id 101 :xmlns "urn:ietf:params:xml:ns:netconf:base:1.0"}
           [:result "5.0"]])
        (is (rel= 5 reply-val :digits 9))))

    (tf/with-forest (tf/new-forest)
      (reset-rpc-msg-id 100)
      (let [module-hid         (tf/add-tree-hiccup yang-ast-hiccup)
            module-bush-before (tf/hid->bush module-hid)
            >>                 (tx-module module-hid)
            module-bush-after  (tf/hid->bush module-hid) ]
        (is= module-bush-before
          [{:tag :module}
           [{:tag :identifier} "calculator"]
           [{:tag :namespace}
            [{:tag :string} "http://brocade.com/ns/calculator"]]
           [{:tag :contact}
            [{:tag :string} "Alan Thompson <athomps@brocade.com>"]]
           [{:tag :description}
            [{:tag :string} "YANG spec for a simple RPN calculator"]]
           [{:tag :revision}
            [{:tag :iso-date} "2017-04-01"]
            [{:tag :description} [{:tag :string} "Prototype 1.0"]]]
           [{:tag :rpc}
            [{:tag :identifier} "add"]
            [{:tag :description} [{:tag :string} "Add 2 numbers"]]
            [{:tag :input}
             [{:tag :leaf}
              [{:tag :identifier} "x"]
              [{:tag :type} [{:tag :identifier} "decimal64"]]]
             [{:tag :leaf}
              [{:tag :identifier} "y"]
              [{:tag :type} [{:tag :identifier} "decimal64"]]]]
            [{:tag :output}
             [{:tag :leaf}
              [{:tag :identifier} "result"]
              [{:tag :type} [{:tag :identifier} "decimal64"]]]]]])
        (is= module-bush-after
          [{:tag         :module,
            :name        :calculator,
            :namespace   "http://brocade.com/ns/calculator",
            :contact     "Alan Thompson <athomps@brocade.com>",
            :description "YANG spec for a simple RPN calculator",
            :revision    "2017-04-01"}
           [{:tag :rpc, :name :add}
            [{:tag :input}
             [{:type :decimal64, :name :x}]
             [{:type :decimal64, :name :y}]]
            [{:tag :output} [{:type :decimal64, :name :result}]]]])))
  ))

;-----------------------------------------------------------------------------
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
                             [:y {:type :decimal64}]]
                            [:output
                             [:leaf {:identifier :result :type :decimal64}]]])
          msg-hid        (tf/add-tree-hiccup
                           [:rpc
                            [:add
                             [:x (str x)]
                             [:y (str y)]]])
          result-promise (promise)
          msg-id         (next-rpc-msg-id)
          ; >>             (swap! rpc-msg-id-map glue {msg-id result-promise})
          >>             (tf/attrs-merge msg-hid
                           {:message-id msg-id
                            :xmlns      "urn:ietf:params:xml:ns:netconf:base:1.0"})
          result-hid     (validate-parse-rpc-tree rpc-schema-hid msg-hid)
          ;(tf/hid->tree result-hid)
          ;    {:attrs {:tag :rpc-reply, :message-id 101, :xmlns "urn:ietf:params:xml:ns:netconf:base:1.0"},
          ;     :kids  [{:attrs {:tag :data}, :value 5.0}]}
          result-value   (tf/find-leaf-value result-hid [:rpc-reply :data])]
         ; result-hid (deref result-promise *rpc-timeout-ms* ::timeout-failure)
         ;(when (instance? Throwable result-hid)
         ;  (throw (RuntimeException. (.getMessage result-hid))))
         ;(when (= ::timeout-failure result)
         ;  (throw (TimeoutException. (format "Timeout Exceed=%s  add: %s %s; " *rpc-timeout-ms* x y))))
         result-value)))

(dotest
  (binding [*rpc-timeout-ms*        100
            *rpc-delay-simulated-ms* 10]
    (reset-rpc-msg-id 100)
    (is (rel= 5 (add 2 3) :digits 9)) ))

;-----------------------------------------------------------------------------
(dotest
  (let [abnf-src            (io/resource "yang3.abnf")
        yang-src            (slurp (io/resource "mul3.yang"))
        parse-and-transform (create-parser-transformer abnf-src yang-tx-map)
        yang-ast-hiccup     (parse-and-transform yang-src)]
       (tf/with-forest (tf/new-forest)
         (reset-rpc-msg-id 100)
         (let [module-hid         (tf/add-tree-hiccup yang-ast-hiccup)
               module-bush-before (tf/hid->bush module-hid)
               >>                 (tx-module module-hid)
               module-bush-after  (tf/hid->bush module-hid)
               schema-hid         (tf/find-hid module-hid [:module :rpc])
               rpc-api-clj        (rpc->api schema-hid)
               call-msg           (rpc-call-marshall schema-hid [2 3 4])
               msg-marshalled-hid (tf/add-tree-hiccup call-msg)
               call-unmarshalled  (rpc-call-unmarshall schema-hid msg-marshalled-hid)
               call-result        (invoke-rpc call-unmarshalled)
               reply-msg          (rpc-reply-marshall schema-hid msg-marshalled-hid call-result)
               reply-hid          (tf/add-tree-hiccup reply-msg)
               reply-val          (reply-unmarshall schema-hid reply-hid) ]
          ;(spyx-pretty module-bush-before)
           (is= module-bush-after
             [{:tag :module,
               :name :calculator,
               :namespace "http://brocade.com/ns/calculator",
               :contact "Alan Thompson <athomps@brocade.com>",
               :description "YANG spec for a simple RPN calculator",
               :revision "2017-04-01"}
              [{:tag :rpc, :name :mul3}
               [{:tag :input}
                [{:type :decimal64, :name :x}]
                [{:type :decimal64, :name :y}]
                [{:type :decimal64, :name :z}]]
               [{:tag :output} [{:type :decimal64, :name :result}]]]] )
           (is=      rpc-api-clj   '(fn fn-mul3 [x y z] (fn-mul3-impl x y z))  )
           (is= (str rpc-api-clj)  "(fn fn-mul3 [x y z] (fn-mul3-impl x y z))" )
           (is= call-msg [:rpc [:mul3 {:message-id 101}
                                [:x "2"] [:y "3"] [:z "4"]]])
           (is (wild-match? {:rpc-fn :*, :args [2.0 3.0 4.000]} call-unmarshalled))
           (is= call-result 24.0)
           (is= reply-msg
             [:rpc-reply {:message-id 101 :xmlns "urn:ietf:params:xml:ns:netconf:base:1.0"}
              [:result "24.0"]])
           (is (rel= 24 reply-val :digits 9))

           ))))
