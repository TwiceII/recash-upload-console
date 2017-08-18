(defproject recash-upload-console "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [proto-repl "0.3.1"]
                 [org.postgresql/postgresql "9.3-1102-jdbc41"]
                 [com.datomic/datomic-pro "0.9.5561"]
                 [vvvvalvalval/datofu "0.1.0"]]
  :main ^:skip-aot recash-upload-console.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all
                       :uberjar-name "recash-upload-console.jar"}})