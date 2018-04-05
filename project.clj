(defproject twttr/ops "2.0.0"
  :description "High-level companion for consuming the Twitter API"
  :url "https://github.com/chbrown/twttr-ops"
  :license {:name "Eclipse Public License"
            :url "https://www.eclipse.org/legal/epl-v10.html"}
  :pom-addition [:developers [:developer
                              [:name "Christopher Brown"]
                              [:email "io@henrian.com"]]]
  :dependencies [[org.clojure/clojure "1.9.0"]
                 [org.clojure/tools.cli "0.3.5"]
                 [org.clojure/tools.logging "0.4.0"]
                 [ch.qos.logback/logback-classic "1.2.3"]
                 [chbrown/data.json "0.2.7"]
                 [fancy "0.1.0"]
                 [twttr "3.0.0-beta2"]
                 [manifold "0.1.6"]
                 [byte-streams "0.2.3" :exclusions [manifold]]
                 [byte-transforms "0.1.4" ]]
  :exclusions [org.clojure/data.json]
  :main twttr.cli
  ; -XX:-UseGCOverheadLimit prevents throwing an OutOfMemoryError if too much
  ; time is spent doing garbage collection; see https://bit.ly/GCOverheadLimit
  ; -XX:+UseConcMarkSweepGC enables the concurrent garbage collector
  ; -Xmx4g sets the maximum (memory) heap footprint to 4 gigabytes
  :jvm-opts ["-XX:-UseGCOverheadLimit" "-Xmx4g"]
  :profiles {:uberjar {:aot :all}
             :dev {:dependencies [[org.clojure/tools.namespace "0.3.0-alpha3"]
                                  [org.clojure/tools.trace "0.7.9"]]
                   :source-paths ["dev"]
                   :repl-options {:init-ns user}}})
