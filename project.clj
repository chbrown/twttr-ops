(defproject twttr/ops "2.0.0"
  :description "High-level companion for consuming the Twitter API"
  :url "https://github.com/chbrown/twttr-ops"
  :license {:name "Eclipse Public License"
            :url "https://www.eclipse.org/legal/epl-v10.html"}
  :deploy-repositories [["releases" :clojars]]
  :pom-addition [:developers [:developer
                              [:name "Christopher Brown"]
                              [:email "io@henrian.com"]]]
  :dependencies [[org.clojure/clojure "1.10.1"]
                 [org.clojure/tools.cli "0.4.2"]
                 [org.clojure/tools.logging "0.5.0"]
                 [ch.qos.logback/logback-classic "1.2.3"]
                 [chbrown/data.json "0.2.7"]
                 [fancy "0.2.3"]
                 [twttr "3.2.2"]
                 [manifold "0.1.8"]
                 [byte-streams "0.2.4" :exclusions [manifold]]
                 [byte-transforms "0.1.4" ]]
  :exclusions [org.clojure/data.json]
  :main twttr.cli
  ; -XX:-UseGCOverheadLimit prevents throwing an OutOfMemoryError if too much
  ; time is spent doing garbage collection; see https://bit.ly/GCOverheadLimit
  ; -XX:+UseConcMarkSweepGC enables the concurrent garbage collector
  ; -Xmx4g sets the maximum (memory) heap footprint to 4 gigabytes
  :jvm-opts ["-XX:-UseGCOverheadLimit" "-Xmx4g"]
  :profiles {:uberjar {:aot :all}
             :dev {:dependencies [[org.clojure/tools.namespace "0.3.1"]
                                  [org.clojure/tools.trace "0.7.10"]]
                   :source-paths ["dev"]
                   :repl-options {:init-ns user}}})
