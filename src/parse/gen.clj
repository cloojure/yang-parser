(ns parse.gen
  (:require
    [clojure.java.io :as io]
    [clojure.string :as str]
    [clojure.tools.reader.edn :as edn]
    [schema.core :as s]
    [tupelo.core :as t]
    [tupelo.misc :as tm]
    [tupelo.schema :as ts]
    [tupelo.string :as tstr]
  ))
(t/refer-tupelo)

; #todo #awt add schema stuff

(s/defn vec-has-tag? :- s/Bool
  [data :- ts/List
   tag  :- s/Keyword ]
  (when data
    (= (first data) tag)))

(defn find-vecs-with-tag
  [data search-tag]
  (when data
    (let [all-vecs (keep-if vector? data)
          keepers  (keep-if #(vec-has-tag? % search-tag) all-vecs)]
      keepers)))

(s/defn get-children :- s/Any
  [data   :- ts/List
   tag    :- s/Keyword ]
  (when data
    (find-vecs-with-tag (rest data) tag)))

(s/defn get-child-strict :- s/Any ; #todo maybe add "...-if-exists" or "... :default nil" variants?
  [data   :- ts/List
   tag    :- s/Keyword ]
  (when data
    (if-let [result (only (get-children data tag))]
      result
      (throw (IllegalArgumentException. (str "nil result found;  tag=" tag "  data=" data))))))

(s/defn get-child :- s/Any
  [data   :- ts/List
   tag    :- s/Keyword ]
  (when data
    (first (get-children data tag))))

(s/defn get-child-value :- s/Any
  [data   :- ts/List
   tag    :- s/Keyword ]
  (when data
    (second (get-child data tag))))

(defn is-basic
  [node]
  (let [node-key (first node)]
    (truthy? (#{:name :namespace :description :prefix :length :uses :config :mandatory
                :unique :key :default :presence :organization :contact :base} node-key))))

(defn proc-name [data tmpl]
  (str/replace tmpl "<<<name>>>"              (or (get-child-value data :name) "")))

(defn proc-description [data tmpl]
  (str/replace tmpl "<<<description>>>"       (or (get-child-value data :description) "")))

(defn proc-presence [data tmpl]
  (str/replace tmpl "<<<presence>>>"          (or (get-child-value data :presence) "")))

(defn proc-namespace [data tmpl]
  (str/replace tmpl "<<<namespace>>>"         (or (get-child-value data :namespace) "")))

(defn proc-prefix [data tmpl]
  (str/replace tmpl "<<<prefix>>>"            (or (get-child-value data :prefix) "")))

(defn proc-organization [data tmpl]
  (str/replace tmpl "<<<organization>>>"      (or (get-child-value data :organization) "")))

(defn proc-contact [data tmpl]
  (str/replace tmpl "<<<contact>>>"           (or (get-child-value data :contact) "")))

(defn proc-base [data tmpl]
  (str/replace tmpl "<<<base>>>"              (or (get-child-value data :base) "")))

(defn proc-revision
  [data-module tmpl-module]
  (let [tmpl-revision (tm/single-quotes->double-quotes "
(defn revision [] { :name          '<<<name>>>'
                    :description   '<<<description>>>' } ) " )
        data-revision     (get-child data-module :revision)
        revision-txt      (it-> tmpl-revision
                            (proc-name         data-revision it)
                            (proc-description  data-revision it))
        ]
    (str/replace tmpl-module "<<<revision>>>" revision-txt)))

(defn proc-identities
  [data-module tmpl-module]
  (let [tmpl (tm/single-quotes->double-quotes "
(def <<<name>>> { :description  '<<<description>>>'
                  :base         '<<<base>>>' } )" )
        identities (get-children data-module :identity)
        identities-txt (str/join
                         (forv [identity identities]
                           (it-> tmpl
                             (proc-name identity it)
                             (proc-base identity it)
                             (proc-description identity it)))) ]
    (str/replace tmpl-module "<<<identities>>>" identities-txt)))

(defn proc-node
  [node]
  (let [[node-key & node-values] node]
    (hash-map node-key
      (apply glue
        (forv [item node-values]
          (if (is-basic item)
            { (first item) (second item) }
            (proc-node item)))))))

(defn parse-leaves
  [data-leaves]
  (let [leaves (get-children data-leaves :leaf) ]
    (forv [leaf leaves]
      {:name        (get-child-value leaf :name)
       :default     (get-child-value leaf :default)
       :base        (get-child-value leaf :base)
       :type        (get-child-value leaf :type)
       :description (get-child-value leaf :description)
       :config      (get-child-value leaf :config)
       :mandatory   (get-child-value leaf :mandatory)
       } )))

(defn proc-rpc-inputs
  [data-rpc tmpl-rpc]
  (let [tmpl-leaf      "<<<name>>> "
        data-leaves     (parse-leaves (get-child data-rpc :rpc-input))
        rpc-inputs-txt  (str/join
                          (forv [data-leaf data-leaves]
                            (it-> tmpl-leaf
                              (str/replace it "<<<name>>>" (grab :name data-leaf)))))
    ]
    rpc-inputs-txt
    (str/replace tmpl-rpc "<<<rpc-inputs>>>" (str/trim rpc-inputs-txt))))
; #todo add [:type [:name "xxx"] [:base "xxx:yyy"]]
; #todo add [:default "xxx"]

(defn proc-rpcs
  [data-module tmpl-module]
  (let [tmpl-rpc (tm/single-quotes->double-quotes "
(defn <<<name>>>
  [<<<rpc-inputs>>>]
  '<<<description>>>'
) \n" )
        data-rpcs (get-children data-module :rpc)
        rpcs-txt  (str/join
                    (forv [data-rpc data-rpcs]
                      (it-> tmpl-rpc
                        (proc-name        data-rpc it)
                        (proc-rpc-inputs  data-rpc it)
                        (proc-description data-rpc it))))]
    (str/replace tmpl-module "<<<rpcs>>>" rpcs-txt)))

(defn proc-containers
  [data-module tmpl-module]
  (let [tmpl (tm/single-quotes->double-quotes "
(defn container-<<<name>>> []
  { :presence       '<<<presence>>>'
    :description    '<<<description>>>'
    :leaves <<<leaves>>>
  } ) " )
        data-containers (get-children data-module :container)
        containers-txt   (str/join
                           (forv [data-container data-containers]
                             (let [data-leaves     (parse-leaves data-container)
                                   data-leaves-str (str \newline (t/pretty-str data-leaves))]
                               (it-> tmpl
                                 (proc-name data-container it)
                                 (proc-description data-container it)
                                 (proc-presence data-container it)
                                 (str/replace it "<<<leaves>>>" (tstr/indent-lines 16 data-leaves-str))
                                 )))) ]
    (str/replace tmpl-module "<<<containers>>>" containers-txt)))

(defn proc-groupings
  [data-module tmpl-module]
  (let [tmpl (tm/single-quotes->double-quotes "
(defn grouping-<<<name>>> []
  { :presence       '<<<presence>>>'
    :description    '<<<description>>>'
    :leaves <<<leaves>>>
  } ) " )
        data-groupings (get-children data-module :grouping)
        groupings-txt   (str/join
                           (forv [data-grouping data-groupings]
                             (let [data-leaves     (parse-leaves data-grouping)
                                   data-leaves-str (str \newline (t/pretty-str data-leaves))]
                               (it-> tmpl
                                 (proc-name data-grouping it)
                                 (proc-description data-grouping it)
                                 (proc-presence data-grouping it)
                                 (str/replace it "<<<leaves>>>" (tstr/indent-lines 16 data-leaves-str))
                                 )))) ]
    (str/replace tmpl-module "<<<groupings>>>" groupings-txt)))

(defn proc-notifications
  [data-module tmpl-module]
  (let [tmpl (tm/single-quotes->double-quotes "
(defn notification-<<<name>>> []
  { :description    '<<<description>>>'
    :leaves         <<<leaves>>>
  } ) " )
        data-notifications  (get-children data-module :notification)
        notifications-txt   (str/join
                              (forv [data-notification data-notifications]
                                (let [data-leaves     (parse-leaves data-notification)
                                      data-leaves-str (str \newline (t/pretty-str data-leaves))]
                                  (it-> tmpl
                                    (proc-name data-notification it)
                                    (proc-description data-notification it)
                                    (str/replace it "<<<leaves>>>" (tstr/indent-lines 16 data-leaves-str))
                                  )))) ]
    (str/replace tmpl-module "<<<notifications>>>" notifications-txt)))

(defn proc-module
  [data-module]
  (assert (= :module (first data-module)))
  (let [tmpl (tm/single-quotes->double-quotes "
; ***** Machine Generated File - Do Not Edit *****

(ns yang.gen.<<<name>>>
  (:require
    [clojure.java.io :as io]
    [clojure.string :as str]
    [tupelo.core :as t]
  ))
(t/refer-tupelo)

(defn namespace      [] '<<<namespace>>>' )
(defn prefix         [] '<<<prefix>>>' )
(defn organization   [] '<<<organization>>>')
(defn contact        [] '<<<contact>>>')
(defn description    [] '<<<description>>>')

<<<revision>>>

;-----------------------------------------------------------------------------
; Identities
<<<identities>>>

;-----------------------------------------------------------------------------
; RPCs
<<<rpcs>>>

;-----------------------------------------------------------------------------
; RPCs
<<<notifications>>>

;-----------------------------------------------------------------------------
; Containers
<<<containers>>>

;-----------------------------------------------------------------------------
; Groupings
<<<groupings>>>

; ***** Machine Generated File - Do Not Edit ***** " )
        module-txt     (it-> tmpl
                         (proc-name data-module it)
                         (proc-namespace data-module it)
                         (proc-prefix data-module it)
                         (proc-organization data-module it)
                         (proc-contact data-module it)
                         (proc-description data-module it)
                         (proc-revision data-module it)
                         (proc-rpcs data-module it)
                         (proc-identities data-module it)

                         (proc-containers data-module it)
                         (proc-groupings data-module it)
                         (proc-notifications data-module it)
                       )
        ]
    module-txt
  ))

(defn proc-file
  [curr-file]
  (let [file-tx  (str "resources/tx/" curr-file ".edn")
        curr-file-out (str/replace curr-file \- \_ )
        file-gen (str "resources/gen/" curr-file-out ".clj")
        _        (printf "loading: %s \n" file-tx)
        data-tx  (edn/read-string (slurp file-tx))
        gen-code (proc-module data-tx)
  ]
    (spit file-gen gen-code)
  ))
