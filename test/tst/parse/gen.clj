(ns tst.parse.gen
  (:use parse.gen
        tupelo.test
        clojure.test )
  (:require
    [clojure.tools.reader.edn :as edn]
    [tupelo.core :as t]
  ))
(t/refer-tupelo)

(comment            ; #todo 2017-5-5 delete this?

  (deftest t-proc-leaf
    (is= (proc-node
           [:leaf
            [:name "coord"]
            [:type [:name "cell-index"]]
            [:description "Coordinate (index) of the tape cell."]])

      {:leaf
       {:name        "coord",
        :type        {:name "cell-index"},
        :description "Coordinate (index) of the tape cell."}})


    (is= (proc-node
           [:leaf
            [:name "symbol"]
            [:type [:name "tape-symbol"] [:length "1"]]
            [:description "Symbol appearing in the tape cell. Blank (empty string) is not allowed here because the 'cell' list only contains non-blank cells."]])
      {:leaf
       {:name        "symbol",
        :type        {:name "tape-symbol", :length "1"},
        :description "Symbol appearing in the tape cell. Blank (empty string) is not allowed here because the 'cell' list only contains non-blank cells."}})
    )

  (deftest t-typedef
    (is= (proc-node
           [:typedef
            [:name "state-index"]
            [:type [:name "uint16"]]
            [:description "Type for indexing states of the control unit."]])
      {:typedef
       {:name        "state-index",
        :type        {:name "uint16"},
        :description "Type for indexing states of the control unit."}} )

    (is= (proc-node
           [:typedef
            [:name "tape-symbol"]
            [:type [:name "string"] [:length "0..1"]]
            [:description
             "Type of symbols appearing in tape cells. A blank is represented as an empty string where necessary."]])
       {:typedef
        {:name "tape-symbol",
         :type {:name "string", :length "0..1"},
         :description "Type of symbols appearing in tape cells. A blank is represented as an empty string where necessary." }} )

    ; #todo need to finalize so it passes.
    #_(is= (proc-node
           [:typedef
            [:name "head-dir"]
            [:type
             [:name "enumeration"]
             [:enum [:name "left"]]
             [:enum [:name "right"]]]

            [:default "right"]
            [:description
             "Possible directions for moving the read/write head, one cell to the left or right (default)."]] )
      {:typedef
       {:name        "head-dir"
        :type        {:name  "enumeration"
                      :enums [ {:name "right"}
                               {:name "left" } ] }
        :default     "right",
        :description "Possible directions for moving the read/write head, one cell to the left or right (default)."}} )
  )

  (def yang-root-names ; #todo
    [; "acme"
     ; "toaster"
  ;    "turing-machine"
     ; "ietf-netconf-acm"
     ; "brocade-beacon"
     ; "brocade-rbridge"
     ; "brocade-terminal"
     ; "yuma-proc"
     ; "yuma-xsd"
     ] )


  (deftest t-bootstrap
    (println "bootstrap - enter")
    (doseq [curr-file yang-root-names]
      (newline)
      (proc-file curr-file)
    ))

)
