(ns applied-science.gedcom-test
  (:require [clojure.test :refer :all]
            [applied-science.gedcom :refer :all]))


(deftest parser
  (testing "gedcom-line"
    (are [input expected] (= expected (into {} (gedcom-line input)))
      "0 FOO"           {:level 0 :tag "FOO"}
      "0 @I1@ FOO data" {:level 0 :label "@I1@" :tag "FOO" :data "data"}
      "0 FOO data"      {:level 0 :tag "FOO" :data "data"}
      "0 FOO @I1@"      {:level 0 :tag "FOO" :data "@I1@"}
      "0 @I1@ FOO__EN data" {:level 0 :label "@I1@" :tag "FOO" :suffix "EN" :data "data"}))

  (testing "gedcom-line-seq"
    (are [input expected]
         (= expected (into {} (->> input (map gedcom-line) gedcom-line-seq first)))
         ["0 FOO one" "1 CONT two" "1 CONC  three"] {:level 0 :tag "FOO" :data "one\ntwo three"})))


;; TODO test non-minimal `samples`


(deftest samples
  (testing "minimal sample"
    (let [[header submitter trailer :as records] (parse-records "samples/MINIMAL555.GED")]
      (testing "sanity check: top-level records are top level"
        (is (every? (comp zero? :level) records)))
      (testing "sanity check: header is a header"
        (is (= "HEAD" (:tag header))))
      (testing "sanity check: trailer is a trailer"
        (is (= "TRLR" (:tag trailer))))
      (testing "this is an official GEDCOM sample"
        (is (= "gedcom.org" (-> header
                                (get "SOUR")
                                first
                                :data))))
      (testing "sample uses expected/supported version"
        (is (= "5.5.5" (-> header
                           (get "GEDC")
                           first
                           (get "VERS")
                           first
                           :data))))
      (testing "sample uses expected/supported charset"
        (is (= "UTF-8" (-> header
                           (get "CHAR")
                           first
                           :data))))
      (testing "sample was submitted by 'someone'"
        (is (and (-> submitter :label some?)
                 (-> submitter :tag #{"SUBM"})))))))
