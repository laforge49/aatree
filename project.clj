(defproject aatree "0.1.0"
  :description "A Clojure library for AA Trees"
  :url "https://github.com/laforge49/aatree"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.7.0"]]
  :aot [aatree.CountedSequence aatree.nodes aatree.map-nodes aatree.vector-nodes aatree.AAMap]
  :warn-on-reflection true
  :profiles {:dev {:dependencies [[collection-check "0.1.6"]]}})
