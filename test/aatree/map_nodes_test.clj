(ns aatree.map-nodes-test
  (:require [clojure.test :refer :all]
            [aatree.nodes :refer :all]
            [aatree.map-nodes :refer :all])
  (:import (clojure.lang MapEntry)))

(def m0 (create-empty-map-node))

(def m1 (insert m0 (new MapEntry "1" 1001)))
(pnodev m1 "m1")
(pnodev (del m1 "1") "m1 - 1")

(def m13 (insert m1 (new MapEntry "3" 1003)))
(pnodev m13 "m13")
(println "m13 level" (.level m13))
(pnodev (del m13 "1") "m13 - 1")
(pnodev (del (del m13 "1") "3") "m13 - -")
(def m123 (insert m13 (new MapEntry "2" 1002)))
(pnodev m123 "m123")
(pnodev (del m123 "1") "m123 - 1")
(pnodev (del m123 "2") "m123 - 2")
(pnodev (del m123 "3") "m123 - 3")
(pnodev (insert m123 (new MapEntry "1" 1001)) "m123 + 1")
(pnodev (insert m123 (new MapEntry "1" 1010)) "m123 + 1")

(println (new-counted-seq m0))
(println (new-counted-seq m1))
(println (new-counted-seq m13))
(println (new-counted-seq m123))
(println (new-counted-reverse-seq m123))
(println (new-map-key-seq m123))
(println (new-map-key-reverse-seq m123))
(println (new-map-value-seq m123))
(println (new-map-value-reverse-seq m123))

(println "")
(def mi (new-counted-iterator m123))
(println (.hasNext mi))
(println (.next mi))
(println (.hasNext mi))
(println (.next mi))
(println (.hasNext mi))
(println (.next mi))
(println (.hasNext mi))
(println (index-of m123 "0"))
(println (index-of m123 "1"))
(println (index-of m123 "2"))
(println (index-of m123 "3"))
(println (index-of m123 "4"))
(println (nth-t2 m123 0))
(println (nth-t2 m123 1))
(println (nth-t2 m123 2))
