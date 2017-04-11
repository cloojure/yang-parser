(ns tst.parse.paths
  (:use parse.paths
        tupelo.test
        clojure.test )
  (:require
    [tupelo.core :as t]
  ))
(t/refer-tupelo)

(def data0
  [:root
   [:a 1 2 3]
   [:b]
   [:c 30] ] )

(deftest t-dowalk
  (is= (dowalk data0)
       [ {:path [:root :a], :value 1}
         {:path [:root :a], :value 2}
         {:path [:root :a], :value 3}
         {:path [:root :b], :value nil}
         {:path [:root :c], :value 30} ] )
  )

(deftest t-find-entries
  (is= (find-entries data0 [:root])
    [ {:path [:root :a], :value 1}
      {:path [:root :a], :value 2}
      {:path [:root :a], :value 3}
      {:path [:root :b], :value nil}
      {:path [:root :c], :value 30} ] )

  (is= (find-entries data0 [:root :a])
    [ {:path [:root :a], :value 1}
      {:path [:root :a], :value 2}
      {:path [:root :a], :value 3} ] )

  (is= (find-entries data0 [:root :b])
    [ {:path [:root :b], :value nil} ] )

  (is= (find-entries data0 [:root :c])
    [ {:path [:root :c], :value 30} ] )
)

(def data-gary
   [:grouping
    [:name "bundle-ethernet-grouping"]
    [:leaf
     [:name "bundle-interface-name"]
     [:type
      [:name "string"]]
     [:description "name of the interface"]]
    [:leaf
     [:name "bundle-interface-description"]
     [:type
      [:name "string"]]
     [:description "interface description should include the type of interface, eg: A-Leaf, P-Leaf, C-Leaf"]]
    [:leaf
     [:name "bundle-interface-mtu"]
     [:type
      [:name "com-att-common-definitiontypes:mtu-size"]]
     [:description "interface MTU"]]
    [:leaf
     [:name "bundle-system-mac"]
     [:type
      [:name "yang:mac-address"]]
     [:description "LACP System-MAC"]]
    [:leaf
     [:name "bundle-load-interval"]
     [:type
      [:name "uint32"]
      [:range
       [:range-simple "0..600"]]]
     [:description "load-interval in seconds"]]]
  )

(deftest t-gary
  (prn  :gary)
  (is=
    (find-entries data-gary [:grouping :leaf :name]))
    [ {:path [:grouping :leaf :name], :value "bundle-interface-name"}
      {:path [:grouping :leaf :name], :value "bundle-interface-description"}
      {:path [:grouping :leaf :name], :value "bundle-interface-mtu"}
      {:path [:grouping :leaf :name], :value "bundle-system-mac"}
      {:path [:grouping :leaf :name], :value "bundle-load-interval"} ] )
