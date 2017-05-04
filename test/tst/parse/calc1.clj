(ns tst.parse.calc1
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
    [tupelo.enlive :as te])
  (:import [java.util.concurrent TimeoutException]
           [java.util List]))
(t/refer-tupelo)


(def ^:dynamic *rpc-timeout-ms* 200)
(def ^:dynamic *rpc-delay-simulated-ms* 30)

(defn rpc-call-1
  [msg]
  (let [result-promise (promise)]
    (future
      (let [rpc-fn-tag (grab :tag msg)
            rpc-fn     (grab rpc-fn-tag rpc-fn-map)
            args       (grab :content msg)]
        (Thread/sleep *rpc-delay-simulated-ms*)
        (deliver result-promise (apply rpc-fn args))))
    result-promise))

(defn add-1 [x y]
  (let [result-promise (rpc-call-1 (te/hiccup->enlive [:add x y]))
        rpc-result     (deref result-promise *rpc-timeout-ms* :timeout-failure)]
    (when (= :timeout-failure rpc-result)
      (throw (TimeoutException. (format "Timeout Exceed=%s  add: %s %s; " *rpc-timeout-ms* x y))))
    rpc-result))

(dotest
  (binding [*rpc-timeout-ms*         200
            *rpc-delay-simulated-ms* 30]
    (is= 5 (spyx (add-1 2 3))))
  (binding [*rpc-timeout-ms*         20
            *rpc-delay-simulated-ms* 30]
    (throws? (add-1 2 3))))

