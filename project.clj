(defproject parse "0.1.0-SNAPSHOT"
  :description "YANG Parsing Library"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
 :dependencies [
   [instaparse "1.4.5"]
   [org.clojure/clojure "1.8.0"]
   [org.clojure/test.check "0.9.0"]
   [org.clojure/core.async           "0.2.395"]
   [prismatic/schema                 "1.1.3"]
   [tupelo "0.9.41"]
 ]
  :profiles { :dev      {:dependencies [[org.clojure/test.check "0.9.0"]] }
              :uberjar  {:aot :all}}
  :global-vars { *warn-on-reflection* false }

  :plugins  [ [lein-codox "0.9.4"] ]
  :codox {:src-dir-uri "http://github.com/cloojure/tupelo/blob/master/"
          :src-linenum-anchor-prefix "L"}
  :deploy-repositories {  "snapshots" :clojars
                          "releases"  :clojars 
                          :sign-releases false }
  :update :daily  ; :always  
  :main ^:skip-aot parse.core
  :target-path      "target/%s"
  :clean-targets  [ "target" ]

  ; "lein test"         will not  run tests marked with the ":slow" metadata
  ; "lein test :slow"   will only run tests marked with the ":slow" metadata
  ; "lein test :all"    will run all  tests (built-in)
  :test-selectors { :default    (complement :slow)
                    :slow       :slow }

  :jvm-opts ^:replace ["-Xms1g" "-Xmx4g" ]
)
