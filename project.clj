(defproject gossip "0.1.0-SNAPSHOT"
  :description "Gossip simulator"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [datascript "0.15.1"]]
  :main ^:skip-aot gossip.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
