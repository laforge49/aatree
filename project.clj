(defproject aatree "0.6.1-SNAPSHOT"
  :description "A Clojure library for AA Trees"
  :url "https://github.com/laforge49/aatree"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.7.0"]
                 [org.clojure/core.async "0.2.374"]
                 [medley "0.7.0"]
                 [org.clojure/tools.logging "0.3.1"]
                 [org.apache.logging.log4j/log4j-core "2.4.1"]
                 [org.apache.logging.log4j/log4j-slf4j-impl "2.4.1"]
                 [org.clojure/core.cache "0.6.4"]
                 ]
  :aot [aatree.CountedSequence aatree.nodes aatree.AAMap aatree.AAVector aatree.AASet]
  :plugins  [[lein-cljfmt "0.3.0"]]
  :profiles {:dev {:dependencies [[collection-check "0.1.6"]]}})
