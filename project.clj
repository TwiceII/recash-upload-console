(defproject recash-upload-console "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.9.0-alpha13"]
                 [org.clojure/test.check "0.9.0"]
                 [org.clojure/data.json "0.2.6"]
                 [orchestra "0.2.0"]
                 [proto-repl "0.3.1"]
                 [clj-time "0.13.0"]
                 [org.clojure/data.xml "0.0.8"]
                 [org.clojure/data.zip "0.1.2"]
                 [org.clojure/data.csv "0.1.3"]
                 [com.stuartsierra/component "0.3.2"]
                 [org.postgresql/postgresql "9.3-1102-jdbc41"]
                 [com.datomic/datomic-pro "0.9.5561"]
                 [vvvvalvalval/datofu "0.1.0"]
                 [io.pedestal/pedestal.log "0.5.1"]]
  :main ^:skip-aot recash-upload-console.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all
                       :uberjar-name "recash-upload-console.jar"}})
