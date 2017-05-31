(ns parse.core
  (use parse.transform)
  (:require
    [clojure.java.io :as io]
    [clojure.set :as set]
    [clojure.string :as str]
    [instaparse.core :as insta]
    [schema.core :as s]
    [tupelo.core :as t]
    [tupelo.x-forest :as tf]
    [tupelo.misc :as tm]
    [tupelo.parse :as tp]
    [tupelo.schema :as tsk]
    [tupelo.string :as ts]
    [tupelo.enlive :as te]))
(t/refer-tupelo)

(def rpc-msg-id (atom 100))
(def rpc-msg-id-map (atom {}))

(defn instaparse-failure? [arg] (instance? instaparse.gll.Failure arg))

(def type-marshall-map {:decimal64 str
                        :int64     str
                        :string    str})

(def type-unmarshall-map {:decimal64 tp/parse-double
                          :int64     tp/parse-long
                          :string    str})

(def rpc-fn-map {:add  (fn fn-add [& args] (apply + args))
                 :mult (fn fn-mult [& args] (apply * args))
                 :pow  (fn fn-power [x y] (Math/pow x y))})

(defn validate-parse-leaf-tree
  "Validate & parse a leaf msg value given a leaf arg-schema (Enlive-format)."
  [arg-schema arg-val]
  (let
    [arg-name-schema (fetch-in arg-schema [:attrs :tag])
     arg-type-schema (fetch-in arg-schema [:attrs :type])
     arg-name-val    (fetch-in arg-val [:attrs :tag])
     xx              (assert (= arg-name-schema arg-name-val))
     ; #todo does not yet verify any attrs;  what rules?
    parser-fn        (grab arg-type-schema type-unmarshall-map)
    parsed-value    (parser-fn (grab :value arg-val))]
    parsed-value)

  #_(try
    (catch Exception e
      (throw (RuntimeException. (str "validate-parse-arg-val: failed for arg-schema=" arg-schema \newline
                                  "  arg-val=" arg-val \newline
                                  "  caused by=" (.getMessage e))))))
  )

(defn validate-parse-rpc-tree
  "Validate & parse a rpc msg valueue given an rpc schema-hid (Enlive-format)."
  [schema-hid rpc-hid]
  (let
    [rpc-tree (tf/hid->tree rpc-hid)
     schema-tree (tf/hid->tree schema-hid)
     >> (assert (= :rpc
                 (fetch-in schema-tree [:attrs :tag])
                 (fetch-in rpc-tree [:attrs :tag])))
     rpc-attrs       (grab :attrs rpc-tree)
     schema-tag     (tf/find-value schema-hid [:rpc :identifier])
     rpc-tag      (it-> rpc-tree
                    (grab :kids it)
                    (only it)
                    (fetch-in it [:attrs :tag]))
     >>              (assert (= schema-tag rpc-tag))
     ; #todo does not yet verify any attrs ;  what rules?

     fn-args-schema  (grab :kids (tf/find-tree schema-hid [:rpc :input]))
     fn-args-rpc   (grab :kids (tf/find-tree rpc-hid [:rpc rpc-tag]))

     parsed-args     (mapv validate-parse-leaf-tree fn-args-schema fn-args-rpc)
     rpc-fn          (grab rpc-tag rpc-fn-map)
     rpc-fn-result   (apply rpc-fn parsed-args)
     result-hid         (tf/add-node (glue rpc-attrs {:tag :rpc-reply})
                          [(tf/add-leaf {:tag :data} rpc-fn-result)]) ]
    result-hid )

  #_(try
    (catch Exception e
      (throw (RuntimeException. (str "validate-parse-rpc: failed for schema-hid=" (pretty-str schema-hid) \newline
                                  "  rpc-hid=" (pretty-str rpc-hid) \newline
                                  "  caused by=" (.getMessage e))))))
)

(def yang-root-names ; #todo
 [   "acme"
     "toaster"
     "turing-machine"
     "ietf-netconf-acm"
     "brocade-beacon"
     "brocade-rbridge"
     "brocade-terminal"
     "yuma-proc"
     "yuma-xsd"
   ])

(defn space-wrap [text] (str \space text \space))

(defn create-abnf-parser-raw
  "Given an ABNF syntax string, creates & returns a parser"
  [abnf-str]
  (let [root-parser    (insta/parser abnf-str :input-format :abnf)
        wrapped-parser (fn fn-wrapped-parser [yang-src]
                         (let [parse-result (try
                                              (root-parser yang-src)
                                              (catch Throwable e ; unlikely
                                                (throw (RuntimeException.
                                                         (str "root-parser: InstaParse failed for \n"
                                                           "yang-src=[[" (clip-str 222 yang-src) "]] \n"
                                                           "caused by=" (.getMessage e))))))]
                           (if (instaparse-failure? parse-result) ; This is the normal failure path
                             (throw (RuntimeException.
                                      (str  "root-parser: InstaParse failed for \n "
                                            "yang-src=[[" (clip-str 222 yang-src) "]] \n"
                                            "caused by=[[" (pr-str parse-result) "]]" )))
                             parse-result)))]
    wrapped-parser))

(defn create-parser-transformer
  "Given an ABNF syntax string, creates & returns a parser that wraps the yang source
  with a leading and trailing space."
  [abnf-str tx-map]
  (let [parser-raw           (create-abnf-parser-raw abnf-str)
        space-wrapped-parser (fn fn-space-wrapped-parser [yang-src]
                               (parser-raw (space-wrap yang-src)))
        parse-and-transform  (fn fn-parse-and-transform [src-text]
                               (let [ast-parse (space-wrapped-parser src-text)
                                     ast-tx    (insta/transform tx-map ast-parse)]
                                 (when (instaparse-failure? ast-tx)
                                   (throw (IllegalArgumentException. (str ast-tx))))
                                 ast-tx))]
    parse-and-transform))

(s/defn leaf-name->attrs
  [leaf-hid :- tf/HID]
  (let [name-kw (keyword (tf/find-value leaf-hid [:leaf :identifier]))
        hid-remove (tf/find-hids leaf-hid [:leaf :identifier])]
    (tf/attrs-merge leaf-hid {:name name-kw})
    (tf/kids-remove leaf-hid hid-remove)))

(s/defn leaf-type->attrs
  [leaf-hid :- tf/HID]
  (tf/hid->tree leaf-hid)
  (let [type-kw (keyword (tf/find-value leaf-hid [:leaf :type :identifier]))
        hid-remove (tf/find-hids leaf-hid [:leaf :type])]
    (tf/attrs-merge leaf-hid {:type type-kw})
    (tf/kids-remove leaf-hid hid-remove)))

(s/defn tx-leaf-type-ident
  "Within a [:leaf ...] node, convert [:type [:identifier 'decimal64']] ->
    {:type :decimal64} "
  [rpc-hid :- tf/HID]
  (tf/validate-hid rpc-hid)
  (let [rpc-leaf-paths (tf/find-paths rpc-hid [:rpc :* :leaf])
        rpc-leaf-hids  (mapv last rpc-leaf-paths) ]
    (run! leaf-type->attrs rpc-leaf-hids)
    (run! leaf-name->attrs rpc-leaf-hids)
    (doseq [hid rpc-leaf-hids]
      (tf/attr-remove hid :tag)
      (tf/kids-remove-all hid))))

(s/defn tx-module-ns
  [module-hid :- tf/HID]
  (let [hids (tf/find-hids module-hid [:module :namespace]) ]
    (when (not-empty? hids)
      (let [ns-hid (only hids)]
        (tf/attrs-merge module-hid {:namespace (tf/find-value ns-hid [:namespace :string])})
        (tf/kids-remove module-hid [ns-hid])))))

(s/defn tx-module-contact
  [module-hid :- tf/HID]
  (let [hids (tf/find-hids module-hid [:module :contact]) ]
    (when (not-empty? hids)
      (let [hid (only hids)]
        (tf/attrs-merge module-hid {:contact (tf/find-value hid [:contact :string])})
        (tf/kids-remove module-hid [hid])))))

(s/defn tx-module-description
  [module-hid :- tf/HID]
  (let [hids (tf/find-hids module-hid [:module :description]) ]
    (when (not-empty? hids)
      (let [hid (only hids)]
        (tf/attrs-merge module-hid {:description (tf/find-value hid [:description :string])})
        (tf/kids-remove module-hid [hid])))))

(s/defn tx-module-revision
  [module-hid :- tf/HID]
  (let [hids (tf/find-hids module-hid [:module :revision]) ]
    (when (not-empty? hids)
      (let [hid (only hids)]
        (tf/attrs-merge module-hid {:revision (tf/find-value hid [:revision :iso-date])})
        (tf/kids-remove module-hid [hid])))))

(s/defn tx-rpc
  [rpc-hid]
  (tf/validate-hid rpc-hid)
  (tx-leaf-type-ident rpc-hid)
  (let [id-hid (tf/find-hid rpc-hid [:rpc :identifier])
        desc-hid (tf/find-hid rpc-hid [:rpc :description])
        rpc-name (keyword (tf/leaf->value id-hid))]
    (tf/attrs-merge rpc-hid {:name rpc-name})
    (tf/kids-remove rpc-hid [id-hid desc-hid])))

(s/defn tx-module ; #todo need Tree datatype for schema
  [module-hid :- tf/HID]
  (let [ident-hid (tf/find-hid module-hid [:module :identifier])
        ident-value (str->kw (tf/leaf->value ident-hid))
        schema-hid (tf/find-hid module-hid [:module :rpc]) ]
    (tf/attrs-merge module-hid {:name ident-value})
    (tf/kids-remove module-hid [ident-hid])
    (tx-module-ns module-hid)
    (tx-module-contact module-hid)
    (tx-module-description module-hid)
    (tx-module-revision module-hid)
    (tx-rpc schema-hid)))

(s/defn rpc->api :- [s/Any]
  [rpc-hid :- tf/HID]
  (let [rpc-tree           (tf/hid->tree rpc-hid)
        rpc-name           (kw->str (tf/hid->attr rpc-hid :name))
        rpc-input-arg-hids (tf/find-hids rpc-hid [:rpc :input :*])
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
                               (append (vec->list (prepend fn-name-impl rpc-arg-syms)))))]
    fn-def ))


(s/defn rpc-call-marshall :- s/Any
  [schema-hid :- tf/HID
   args :- [s/Any]]
  (let [schema-tree           (tf/hid->tree schema-hid)
        schema-fn-name        (tf/hid->attr schema-hid :name)
        schema-input-arg-hids (tf/find-hids schema-hid [:rpc :input :*])
        >>                    (assert (= (count args) (count schema-input-arg-hids)))
        marshalled-args       (forv [[hid arg] (mapv vector schema-input-arg-hids args)]
                                (let [arg-name-kw    (tf/hid->attr hid :name)
                                      arg-type       (tf/hid->attr hid :type)
                                      marshall-fn    (fetch type-marshall-map arg-type)
                                      marshalled-arg [arg-name-kw (marshall-fn arg)] ]
                                  marshalled-arg))
        rpc-msg-id            (swap! rpc-msg-id inc)
        msg-hiccup            [:rpc (glue [schema-fn-name {:message-id rpc-msg-id}]
                                      marshalled-args)]]
    msg-hiccup))

(s/defn rpc-call-unmarshall-args
  [schema-arg-trees :- [tsk/KeyMap]
   msg-arg-trees    :- [tsk/KeyMap]]
  (let [args (map-with [schema-tree   schema-arg-trees
                        msg-tree      msg-arg-trees]
               (let [schema-arg-name     (fetch-in schema-tree [:attrs :name])
                     schema-arg-type     (fetch-in schema-tree [:attrs :type])
                     schema-arg-parse-fn (grab schema-arg-type type-unmarshall-map)
                     msg-arg-name        (fetch-in msg-tree [:attrs :tag])
                     >>                  (assert (= msg-arg-name schema-arg-name))
                     msg-arg-value-raw   (grab :value msg-tree)
                     msg-arg-value       (schema-arg-parse-fn msg-arg-value-raw)]
                 msg-arg-value))]
    args))

(s/defn rpc-call-unmarshall :- s/Any
  [schema-hid :- tf/HID
   msg-hid :- tf/HID ]
  (assert (= :rpc (tf/hid->attr schema-hid :tag) (tf/hid->attr msg-hid :tag)))
  (let-spy-pretty
    [msg-tree                  (tf/hid->tree msg-hid)
     schema-tree               (tf/hid->tree schema-hid)
     schema-fn-name            (tf/hid->attr schema-hid :name)
     msg-fn-name               (tf/hid->attr (tf/find-hid msg-hid [:rpc :*]) :tag)
     >>                        (assert (= schema-fn-name msg-fn-name))
     schema-input-hids         (tf/find-hids schema-hid [:rpc :input :*])
     msg-arg-hids              (tf/find-hids msg-hid [:rpc :* :*])
     >>                        (assert (= (count schema-input-hids) (count msg-arg-hids)))

     schema-arg-trees          (mapv tf/hid->tree schema-input-hids)
     msg-arg-trees             (mapv tf/hid->tree msg-arg-hids)
     args                      (rpc-call-unmarshall-args schema-arg-trees msg-arg-trees)
     rpc-fn                    (grab msg-fn-name rpc-fn-map)
     rpc-call-unmarshalled-map {:rpc-fn rpc-fn
                                :args   args}]
    rpc-call-unmarshalled-map))

(s/defn rpc-reply-marshall :- s/Any
  [schema-hid :- tf/HID
   msg-hid :- tf/HID
   result :- s/Any]
  (let
    [rpc-tree          (tf/hid->tree schema-hid)
     msg-tree          (tf/hid->tree msg-hid)
     msg-attrs         (it-> (grab :kids msg-tree)
                         (only it)
                         (grab :attrs it))
     reply-attrs       (glue {:xmlns "urn:ietf:params:xml:ns:netconf:base:1.0"}
                         (submap-by-keys msg-attrs #{:message-id}))
     schema-reply-tree (only (grab :kids (tf/find-tree schema-hid [:rpc :output])))
     reply-type        (fetch-in schema-reply-tree [:attrs :type])
     marshall-fn       (fetch type-marshall-map reply-type)
     reply-hiccup      [:rpc-reply reply-attrs [:result (marshall-fn result)]]]
    reply-hiccup))

(s/defn invoke-rpc :- s/Any
  [rpc-call-unmarshalled-map :- tsk/Map]
  (let [rpc-fn     (grab :rpc-fn rpc-call-unmarshalled-map)
        args       (grab :args rpc-call-unmarshalled-map)
        rpc-result (apply rpc-fn args)]
    rpc-result))

(s/defn reply-unmarshall :- s/Any
  [schema-hid :- tf/HID
   reply-hid  :- tf/HID]
  (let [schema-tree       (tf/hid->tree schema-hid)
        reply-tree        (tf/hid->tree reply-hid)
        >>                (assert (= :rpc-reply (fetch-in reply-tree [:attrs :tag])))
        result-tree       (it-> reply-tree
                            (grab :kids it)
                            (only it))
        result-unparsed   (grab :value result-tree)

        schema-reply-tree (only (grab :kids (tf/find-tree schema-hid [:rpc :output])))
        reply-type        (fetch-in schema-reply-tree [:attrs :type])
        unmarshall-fn     (fetch type-unmarshall-map reply-type)
        result-parsed     (unmarshall-fn result-unparsed)]
    result-parsed))
