(ns aatree.transparent-transcription-test
  (:require [clojure.test :refer :all]
            [aatree.core :refer :all]
            [aatree.yearling :refer :all])
  (:import (java.io File)))

(set! *warn-on-reflection* true)

(comment

  (deftest transparent-transcription
    (let [file-a (File. "transcription-a.yearling")
          file-b (File. "transcription-b.yearling")
          _ (.delete file-a)
          _ (.delete file-b)
          opts-a (yearling-open file-a)
          opts-b (yearling-open file-b)
          _ (db-update (fn [aamap opts]
                         (assoc aamap :v [1 2 3]))
                       opts-a)
          va (:v (db-get-sorted-map opts-a))
          _ (is (= "aatree.AAVector" (.getName (class va))))
          va-opts (aa-opts va)
          ^File va-file (:db-file va-opts)
          va-file-name (.toString va-file)
          _ (is (= va-file-name (.toString file-a)))
          _ (db-update (fn [aamap opts]
                         (assoc aamap :v va))
                       opts-b)
          vb (:v (db-get-sorted-map opts-b))
          _ (is (= "aatree.AAVector" (.getName (class vb))))
          vb-opts (aa-opts vb)
          ^File vb-file (:db-file vb-opts)
          vb-file-name (.toString vb-file)
          _ (is (= vb-file-name (.toString file-b)))
          _ (db-close opts-a)
          _ (db-close opts-b)
          ])

    (Thread/sleep 200))
  )