(ns parse.paths
  (:require
    [tupelo.core :as t]
    [tupelo.schema :as tsch]
  ))
(t/refer-tupelo)

(defn- walk* [walk-result path data]
  ; (newline)
  ; (println "enter:  path=" path "   data" data)
  (if-not (sequential? data)
    (swap! walk-result t/append {:path path :value data})
    (let [tag      (first data)
          contents (next data)
          new-path (t/append path tag) ]
      (if (nil? contents)
        (swap! walk-result t/append {:path new-path :value nil})
        (doseq [elem contents]
          (walk* walk-result new-path elem))))))

; #todo error checking: data is in hiccup form
(defn dowalk [data]
  (let [walk-result (atom [])]
    (walk* walk-result [] data)
    @walk-result))

(defn path-has-subpath? [path subpath]
  (let [len-path    (count path)
        len-subpath (count subpath)]
    (and
      (<= len-subpath len-path)
      (= subpath (take len-subpath path)))))

(defn entry-has-subpath? [entry subpath]
  (let [entry-path  (grab :path entry) ]
    entry-path
    (path-has-subpath? entry-path subpath)))

; #todo error checking: data is in hiccup form
(defn find-entries [data subpath]
  (let [entries (dowalk data)]
    (filterv #(entry-has-subpath? %  subpath) entries)
    ))

