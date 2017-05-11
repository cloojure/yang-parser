(ns parse.transform
  (:require
    [clojure.java.io :as io]
    [clojure.set :as set]
    [clojure.string :as str]
    [clojure.walk :as walk]
    [instaparse.core :as insta]
    [schema.core :as s]
    [tupelo.core :as t]
    [tupelo.enlive :as te]
    [tupelo.misc :as tm]
    [tupelo.schema :as tsk]
    [tupelo.string :as ts]
    [tupelo.x-forest :as tf]))
(t/refer-tupelo)

(defn container? [tree] (= :container (grab :tag tree)))
(defn uses? [tree] (= :uses (grab :tag tree)))
(defn leaf? [tree] (= :leaf (grab :tag tree)))
(defn description? [tree] (= :description (grab :tag tree)))
(defn identifier? [tree] (= :identifier (grab :tag tree)))
(defn input? [tree] (= :input (grab :tag tree)))
(defn output? [tree] (= :output (grab :tag tree)))
(defn type? [tree] (= :type (grab :tag tree)))
(defn enlive-tree-node? [x] (and (map? x)
                              (t/contains-key? x :tag)
                              (t/contains-key? x :attrs)
                              (t/contains-key? x :content)))

(defn grouping-prune
  "Remove :identifier & :description children from grouping."
  [grouping]
  (let [remove-tags  (fn [tree] (or (identifier? tree) (description? tree)))
        new-contents (drop-if remove-tags (grab :content grouping))
        result       (glue grouping {:content new-contents})]
    result))

(defn container-with-uses? [tree]
  (truthy?
    (when (container? tree)
      (let [children (grab :content tree)
            uses-kids (keep-if uses? children)
            result (not-empty? uses-kids) ]
        result ))))

(defn uses-replace [groupings-map tree-in]
  (if-not (container-with-uses? tree-in)
    tree-in
    (let [
          children      (grab :content tree-in)
          non-uses-kids (drop-if uses? children)
          uses-kid      (only (keep-if uses? children)) ; #todo can be only 1 for now
          grouping-name (only (grab :content (only (grab :content uses-kid))))
          uses-content  (grab :content (grab grouping-name groupings-map))
          content-out   (glue non-uses-kids uses-content)
          tree-out      (glue tree-in {:content content-out})]
      tree-out)))

(defn tx-uses [ast]
  ; #todo require grouping at top level for now
  (let [ast-enlive  ast ; #todo move to caller
        groupings            (te/find-tree ast-enlive [:module :grouping])
        groupings-subtree    (mapv #(grab :subtree %) groupings)


        groupings-map        (apply glue {}
                               (forv [grouping-subtree groupings-subtree]
                                 (let [identifier     (te/get-leaf grouping-subtree [:grouping :identifier])
                                       subtree-pruned (grouping-prune grouping-subtree)]
                                   {identifier subtree-pruned})))
        uses-trees           (te/find-tree ast-enlive [:module :** :uses])
        uses-replace-wrapper (fn [x]
                               (if (enlive-tree-node? x)
                                 (uses-replace groupings-map x)
                                 x))
        ast-postwalk        (walk/postwalk uses-replace-wrapper ast-enlive) ]
    ; (spyx-pretty (enlive->hiccup ast-postwalk))
    ast-postwalk ))

(defn no-label [arg] arg) ; aka identity

(defn fn-import [arg]
  ; #todo ignore prefix for now
)

; An identifier MUST NOT start with (('X'|'x') ('M'|'m') ('L'|'l')) (i.e. 'xml' in any case)
(defn fn-identifier [& args]
  (let [result  (str/join args)
        first-3 (str/lower-case (strcat (take 3 result))) ]
    (when (= "xml" first-3)
      (throw (IllegalArgumentException. (format "Identifier cannot begin with 'xml': %s " result))))
    [:identifier result] ))
(defn fn-string [& args] [:string (tm/collapse-whitespace (str/join args))] )
(defn fn-rpc [& args] (prepend :rpc args))

(defn yang-transform
  [parse-tree]
  (insta/transform
    {
     :prefix         (fn fn-prefix [arg] [:prefix (second arg)])
     :identifier     fn-identifier
     :string         fn-string
     :integer        (fn fn-integer [arg] [:integer (java.lang.Integer. arg)])
     :boolean        (fn fn-boolean [arg] [:boolean (java.lang.Boolean. arg)])
     :namespace      (fn fn-namespace [arg] [:namespace arg])
     :organization   (fn fn-organization [arg] [:organization arg])
     :contact        (fn fn-contact [arg] [:contact arg])
     :description    (fn fn-description [arg] [:description arg])
     :presence       (fn fn-presence [arg] [:presence arg])
     :revision       (fn fn-revision [& args] (prepend :revision args))
     :iso-date       (fn fn-iso-date [& args] [:iso-date (str/join args)])
     :reference      (fn fn-reference [arg] [:reference arg])
     :identity       (fn fn-identity [& args] (prepend :identity args))
     :typedef        (fn fn-typedef [& args] (prepend :typedef args))
     :container      (fn fn-container [& args] (prepend :container args))
     ;:grouping          #todo make sure no cycles via uses, import, include
     ;:import            #todo make sure no cycles via uses, import, include
     :rpc            fn-rpc
     :rpc-input      (fn fn-input [& args] (prepend :input args))
     :rpc-output     (fn fn-input [& args] (prepend :output args))


     :base           (fn fn-base [arg] [:base arg])

     :error-message  (fn fn-description [arg] [:error-message (tm/collapse-whitespace arg)])
     :length         (fn fn-length [arg] [:length arg])

     :enum-simple    (fn fn-enum-simple [& args] [:enum [:name (first args)]])
     :enum-composite (fn fn-enum-composite [& args]
                       (let [name    (first args)
                             content (rest args)]
                         (t/prepend :enum [:name name] content)))

     :type-simple    (fn fn-type-simple [& args] (t/prepend :type args))
     :type-composite (fn fn-type-composite [& args] (t/prepend :type args))
     :leaf           (fn fn-leaf [& args] (t/prepend :leaf args))
     } parse-tree))


(s/defn leaf-name->attrs
  [leaf-hid :- tf/HID]
  (let [
        name-leaf (tf/find-leaf leaf-hid [:leaf :identifier] :*)
        name-kw   (keyword (only (grab :content name-leaf)))]
    (tf/merge-attrs leaf-hid {:name name-kw})))

(s/defn leaf-type->attrs
  [leaf-hid :- tf/HID]
  (let [
        type-leaf (tf/find-leaf leaf-hid [:leaf :type :identifier] :*)
        type-kw   (keyword (only (grab :content type-leaf)))]
    (tf/merge-attrs leaf-hid {:type type-kw})))

(s/defn tx-leaf-type-ident
  "Wihtin a [:leaf ...] node, convert [:type [:identifier 'decimal64']] ->
    {:type :decimal64} "
  [rpc-hid :- tf/HID]
  (tf/validate-hid rpc-hid)
  (let [rpc-leaf-paths (tf/find-paths rpc-hid [:rpc :* :leaf])
        rpc-leaf-hids  (mapv last rpc-leaf-paths) ]
    (run! leaf-type->attrs rpc-leaf-hids)
    (run! leaf-name->attrs rpc-leaf-hids)
    (doseq [hid rpc-leaf-hids]
      (tf/remove-all-kids hid))))

(s/defn tx-rpc
  [rpc-hid]
  (tf/validate-hid rpc-hid)
  (let [name-leaf (tf/find-leaf rpc-hid [:rpc :identifier] :*)
        name-kw    (keyword (only (grab :content name-leaf)))
        name-attrs {:name name-kw} ]
    (tf/merge-attrs rpc-hid name-attrs))
  (let [hids-to-remove #{(tf/find-hid rpc-hid [:rpc :identifier])
                         (tf/find-hid rpc-hid [:rpc :description])}]
    (tf/remove-kids rpc-hid hids-to-remove)
    (tx-leaf-type-ident rpc-hid)))

(s/defn rpc->api :- [s/Any]
  [rpc-hid :- tf/HID]
  (let [rpc-tree           (tf/hid->tree rpc-hid)
        rpc-name           (name (fetch-in rpc-tree [:attrs :name]))
        rpc-input-hid      (tf/find-hid rpc-hid [:rpc :input])
        rpc-input-arg-hids (grab :kids (tf/hid->node rpc-input-hid))
        rpc-arg-syms       (forv [hid rpc-input-arg-hids]
                             (it-> hid
                               (tf/hid->tree it)
                               (fetch-in it [:attrs :name])
                               (kw->sym it)))
        fn-name            (symbol (str "fn-" rpc-name))
        fn-name-impl       (symbol (str fn-name "-impl"))
        fn-def             (vec->list
                             (-> '(fn)
                               (append fn-name rpc-arg-syms)
                               (append (vec->list (prepend fn-name-impl rpc-arg-syms))))) ]
    fn-def ))
