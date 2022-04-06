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



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Testing official GEDCOM samples from https://www.gedcom.org/samples.html


(deftest sample-minimal
  (testing "The Minimal GEDCOM 5.5.5 File"
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
                                first :data))))
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


;; TODO test non-minimal `samples`


(deftest sample-SSMARR
  (testing "Same-Sex Marriage Example GEDCOM File"
    (let [[_header
           _address
           john-smith
           steven-stevens
           marriage
           _trailer] (parse-records "samples/SSMARR.GED")]
      (testing "john smith birth info"
        (is (= "1 Sep 1991"
               (-> john-smith
                   (get "BIRT") first
                   (get "DATE") first :data)))
        (is (= "Philadelphia, Philadelphia, Pennsylvania, United States of America"
               (-> john-smith
                   (get "BIRT") first
                   (get "PLAC") first :data))))
      (testing "steven stevens birth info"
        (is (= "8 Aug 1988"
               (-> steven-stevens
                   (get "BIRT") first
                   (get "DATE") first :data)))
        (is (= "Seattle, King, Washington, United States of America"
               (-> steven-stevens
                   (get "BIRT") first
                   (get "PLAC") first :data))))
      (testing "john smith personal info"
        (is (= "M" ;; male
               (-> john-smith
                   (get "SEX") first :data)))
        (is (= "John"
               (-> john-smith
                   (get "NAME") first
                   (get "GIVN") first :data)))
        (is (= "Smith"
               (-> john-smith
                   (get "NAME") first
                   (get "SURN") first :data))))
      (testing "steven stevens personal info"
        (is (= "M" ;; male
               (-> steven-stevens
                   (get "SEX") first :data)))
        (is (= "Steven"
               (-> steven-stevens
                   (get "NAME") first
                   (get "GIVN") first :data)))
        (is (= "Stevens"
               (-> steven-stevens
                   (get "NAME") first
                   (get "SURN") first :data))))
      (testing "smith/stevens marriage info"
        (is (= "26 Jun 2015"
               (-> marriage (get "MARR") first
                   (get "DATE") first :data)))
        (is (= "Portland, Mutnomah, Oregon, United States of America"
               (-> marriage (get "MARR") first
                   (get "PLAC") first :data)))
        (is (some? (:data (first (get marriage "HUSB")))))
        ;; format forces one of the men to be "wife" :/
        (is (some? (:data (first (get marriage "WIFE")))))))))


(deftest sample-REMARR
  (testing "Remarriage Example GEDCOM File"
    ;; TODO samples/SSMARR.GED
    ))


(deftest sample-5.5.5-spec
  (testing "The GEDCOM 5.5.5 Specification Sample GEDCOM File (UTF-8)"
    ;; TODO samples/555SAMPLE.GED
    ))
