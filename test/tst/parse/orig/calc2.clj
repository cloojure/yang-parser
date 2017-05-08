(ns tst.parse.orig.calc2
  (:use parse.orig.core
        parse.orig.transform
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
    [tupelo.x-forest :as tf]
    [tupelo.enlive :as te])
  (:import [java.util.concurrent TimeoutException]
           [java.util List]))
(t/refer-tupelo)

(def ^:dynamic *rpc-timeout-ms* 200)
(def ^:dynamic *rpc-delay-simulated-ms* 30)

(def rpc-schema
  (tf/hiccup->enlive [:rpc
                   [:identifier "add"]
                   [:description [:string "Add 2 numbers"]]
                   [:input
                    [:leaf [:identifier "x"] [:type [:identifier "decimal64"]]]
                    [:leaf [:identifier "y"] [:type [:identifier "decimal64"]]]]
                   [:output
                    [:leaf [:identifier "result"] [:type [:identifier "decimal64"]]]]]))

(def rpc-msg-id (atom 100))
(def rpc-deliver-map (atom {}))

(s/defn rpc-call-2 :- s/Any
  [msg :- tsk/KeyMap]
  (let [rpc-msg-id     (swap! rpc-msg-id inc)
        msg            (glue msg {:attrs {:message-id rpc-msg-id
                                          :xmlns      "urn:ietf:params:xml:ns:netconf:base:1.0"}})
        result-promise (promise)]
    (swap! rpc-deliver-map glue {rpc-msg-id result-promise}) ;  #todo temp!

    (future         ; Simulate calling out to http server in another thread
      (try
        (Thread/sleep *rpc-delay-simulated-ms*) ; simulated network delay
        (let [rpc-result        (validate-parse-rpc rpc-schema msg)
              rpc-reply-msg-id  (fetch-in rpc-result [:attrs :message-id])
              fpc-reply-promise (grab rpc-reply-msg-id @rpc-deliver-map)]
          (deliver fpc-reply-promise rpc-result))
        (catch Exception e
          (deliver result-promise ; deliver any exception to caller
            (RuntimeException. (str "rpc-call-2: failed  msg=" msg \newline
                                 "  caused by=" (.getMessage e)))))))

    ; return promise to caller immediately
    result-promise ))

(defn add-2 [x y]
  (let [result-promise (rpc-call-2
                         (tf/hiccup->enlive
                           [:rpc
                            [:add {:xmlns "my-own-ns/v1"}
                             [:x (str x)]
                             [:y (str y)]]]))
        rpc-result     (deref result-promise *rpc-timeout-ms* :timeout-failure)
        _              (when (instance? Throwable rpc-result)
                         (throw (RuntimeException. (.getMessage rpc-result))))
        result         (te/get-leaf rpc-result [:rpc-reply :data])]
    (when (= :timeout-failure rpc-result)
      (throw (TimeoutException. (format "Timeout Exceed=%s  add: %s %s; " *rpc-timeout-ms* x y))))
    result))

(dotest
  (binding [*rpc-timeout-ms*         300
            *rpc-delay-simulated-ms* 10]
    (reset! rpc-msg-id 100)
    (is (rel= 5 (add-2 2 3) :digits 9))))

