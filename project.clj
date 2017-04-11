(defproject parse "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
 :dependencies [
   [org.clojure/clojure "1.8.0"]
   [org.clojure/test.check "0.9.0"]
   [instaparse "1.4.5"]
   [potemkin "0.4.3"]
   [tupelo "0.9.34"]
 ]

  ; "lein test"         will not  run tests marked with the ":slow" metadata
  ; "lein test :slow"   will only run tests marked with the ":slow" metadata
  ; "lein test :all"    will run all  tests (built-in)
  :test-selectors { :default    (complement :slow)
                    :slow       :slow }

  :main ^:skip-aot parse.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}}
  :jvm-opts ^:replace ["-Xms1g" "-Xmx4g" ]
)
