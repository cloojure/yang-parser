(ns parse.transform
  (:require
    [clojure.java.io :as io]
    [clojure.set :as set]
    [clojure.string :as str]
    [instaparse.core :as insta]
    [schema.core :as s]
    [tupelo.core :as t]
    [tupelo.misc :as tm]
    [tupelo.schema :as tsk]
    [tupelo.string :as ts]
    ))
(t/refer-tupelo)


; An identifier MUST NOT start with (('X'|'x') ('M'|'m') ('L'|'l')) (i.e. 'xml' in any case)
(defn fn-identifier [& args]
  (let [result  (str/join args)
        first-3 (str/lower-case (strcat (take 3 result))) ]
    (when (= "xml" first-3)
      (throw (IllegalArgumentException. (format "Identifier cannot begin with 'xml': %s " result))))
    [:identifier result] ))


(defn fn-string [& args] [:string (str/join args) ] )
(defn fn-boolean [arg] (java.lang.Boolean. arg))
(defn fn-base [[_ name]] [:base name])


(defn yang-transform
  [parse-tree]
  (insta/transform
    {
     :string-compound str
     :string-simple   str
     :string          fn-string
     :identifier      fn-identifier
     :boolean         fn-boolean
     :base            fn-base
     :namespace       (fn fn-namespace [arg] [:namespace arg])
     :prefix          (fn fn-prefix [arg] [:prefix arg])
     :organization    (fn fn-organization [arg] [:organization arg])
     :contact         (fn fn-contact [arg] [:contact arg])
     :description     (fn fn-description [arg] [:description arg])
     :revision        (fn fn-revision [& args]
                        (spy :100 args)
                        (prepend :revision args))
     :iso-date        (fn fn-iso-date [& args]
                        (spy :101 args)
                        [:iso-date (str/join args)])
     :reference       (fn fn-reference [& args]
                        (spy :102 args)
                        [:reference (vec args)])

     :date-arg        (fn fn-name-arg [arg] [:name arg])
     :error-message   (fn fn-description [arg] [:error-message (tm/collapse-whitespace arg)])
     :length          (fn fn-length [arg] [:length arg])

     :enum-simple     (fn fn-enum-simple [& args] [:enum [:name (first args)]])
     :enum-composite  (fn fn-enum-composite [& args]
                        (let [name    (first args)
                              content (rest args)]
                          (t/prepend :enum [:name name] content)))

     :type-simple     (fn fn-type-simple [& args]
                        [:type [:name (first args)]])
     :type-composite  (fn fn-type-composite [& args]
                        (let [name    (first args)
                              content (rest args)]
                          (t/prepend :type [:name name] content)))
     } parse-tree))

