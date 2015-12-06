(ns aatree.transparent-transcription-test
  (:require [clojure.test :refer :all]
            [aatree.core :refer :all]
            [aatree.yearling :refer :all]
            [aatree.closer-trait :refer :all])
  (:import (java.io File)))

(set! *warn-on-reflection* true)

(deftest transparent-transcription
  (let [file-a (File. "transcription-a.yearling")
        file-b (File. "transcription-b.yearling")
        _ (.delete file-a)
        _ (.delete file-b)
        yearling-a (yearling-open file-a)
        yearling-b (yearling-open file-b)
        _ (db-update yearling-a
                     (fn [db]
                       (update-assoc-in! db [:app-map :v] [1 2 3])))
        va (db-get-in yearling-a [:app-map :v])
        _ (is (= "aatree.AAVector" (.getName (class va))))
        va-opts (aa-opts va)
        ^File va-file (:db-file va-opts)
        va-file-name (.toString va-file)
        _ (is (= va-file-name (.toString file-a)))
        _ (db-update yearling-b
                     (fn [db]
                       (update-assoc-in! db [:app-map :v] va)))
        vb (db-get-in yearling-b [:app-map :v])
        _ (is (= "aatree.AAVector" (.getName (class vb))))
        vb-opts (aa-opts vb)
        ^File vb-file (:db-file vb-opts)
        vb-file-name (.toString vb-file)
        _ (is (= vb-file-name (.toString file-b)))
        _ (close-components yearling-a)
        _ (close-components yearling-b)
        ])

  (Thread/sleep 200))
