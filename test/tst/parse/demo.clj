(ns tst.parse.demo
  (:use parse.core
        tupelo.test
        clojure.test)
  (:require
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
(dotest
  (let [yp (create-yang-parser "yang3.abnf")
        s1 (ts/single-quotes->double-quotes " 'hello'")
        r1a (yp s1)
        r1b (yang-transform r1a)

        s2 (ts/single-quotes->double-quotes " name" )
        r2a (yp s2)
        r2b (yang-transform r2a)
        ]
    (is= [:file [:tokens [:token [:string "h" "e" "l" "l" "o"]]]] (spyx r1a))
    (is= [:file [:tokens [:token [:string "hello"]]]] (spyx r1b))

    (is= [:file [:tokens [:token [:identifier "n" "a" "m" "e"]]]] (spyx r2a))
    (is= [:file [:tokens [:token [:identifier "name"]]]] (spyx r2b))
    )
  )
