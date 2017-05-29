(ns tst.parse._bootstrap
  "This namespace is used to perform one-time tasks during testing, such as printing the
  Clojure version."
  (:use tupelo.test)
  (:require
    [schema.core :as s]
    [tupelo.core :as t] ))
(t/refer-tupelo)
(t/print-versions)

; Prismatic Schema type definitions
(s/set-fn-validation! true) ; enforce fn schemas

