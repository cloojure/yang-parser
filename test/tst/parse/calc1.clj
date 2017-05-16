(ns tst.parse.calc1
  (:use parse.core
        parse.transform
        tupelo.test
        clojure.test)
  (:require
    [clojure.string :as str]
    [instaparse.core :as insta]
    [schema.core :as s]
    [tupelo.core :as t]
    [tupelo.parse :as tp]
    [tupelo.schema :as tsk]
    [tupelo.string :as ts]
    [tupelo.x-forest :as tf]
    )
  (:import [java.util.concurrent TimeoutException]
           [java.util List]))
(t/refer-tupelo)


(def ^:dynamic *rpc-timeout-ms* 200)
(def ^:dynamic *rpc-delay-simulated-ms* 30)

(defn add-impl
  [msg]
  (let [result-promise (promise)]
    (future
      (let [rpc-fn-tag (grab :tag msg)
            rpc-fn     (grab rpc-fn-tag rpc-fn-map)
            args       (grab :content msg)]
        (Thread/sleep *rpc-delay-simulated-ms*)
        (deliver result-promise (apply rpc-fn args))))
    result-promise))

(defn add [x y]
  (let [result-promise (add-impl (tf/hiccup->enlive [:add x y]))
        rpc-result     (deref result-promise *rpc-timeout-ms* ::timeout-failure)]
    (when (= ::timeout-failure rpc-result)
      (throw (TimeoutException. (format "Timeout Exceed=%s  add: %s %s; " *rpc-timeout-ms* x y))))
    rpc-result))

(dotest
  (binding [*rpc-timeout-ms*         200
            *rpc-delay-simulated-ms* 30]
    (is= 5 (add 2 3)))
  (binding [*rpc-timeout-ms*         20
            *rpc-delay-simulated-ms* 30]
    (throws? (add 2 3))))

