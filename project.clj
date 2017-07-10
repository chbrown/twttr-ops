(defproject twttr/ops "2.0.0"
  :description "High-level companion for consuming the Twitter API"
  :url "https://github.com/chbrown/twttr-ops"
  :license {:name "Eclipse Public License"
            :url "https://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [org.clojure/tools.cli "0.3.5"]
                 [org.clojure/tools.logging "0.4.0"]
                 [ch.qos.logback/logback-classic "1.2.3"]
                 [chbrown/data.json "0.2.7"]
                 [twttr "2.0.0"]
                 [manifold "0.1.6"]
                 [byte-streams "0.2.3" :exclusions [manifold]]
                 [byte-transforms "0.1.4" ]]
  :exclusions [org.clojure/data.json]
  :main twttr.cli
  :profiles {:uberjar {:aot :all}
             :dev {:dependencies [[org.clojure/tools.namespace "0.3.0-alpha3"]
                                  [org.clojure/tools.trace "0.7.9"]]
                   :source-paths ["dev" "src"]
                   :repl-options {:init-ns user}}})
