(ns applied-science.gedcom
  (:require [clojure.java.io :as io])
  (:import org.apache.commons.io.input.BOMInputStream
           java.io.InputStreamReader))


(defn gedcom-line
  "Parse a GedcomLine record from a string."
  [line]
  (when (seq line)
    (let [[_ level label tag suffix data] (re-matches #"(?s)\s*(\d)(?:\s(@[^@]+@))?\s(\w+?)(?:__(\w+))?(?:\s(.*))?" line)]
      (cond-> {:level (Integer. level)}
        label  (assoc :label  label)
        tag    (assoc :tag    tag)
        data   (assoc :data   data)
        suffix (assoc :suffix suffix)))))


(defn gedcom-line-seq
  "Read a GEDCOM line from a line sequence returning a GedcomLine record.
  Lines can be in the following formats:
    level label tag data
    level tag data
    level tag data\\r(level+1) CONT data
    level tag data\\r(level+1) CONC data"
  [lines]
  (lazy-seq (loop [line  (first lines)
                   lines (rest  lines)]
              (when line
                (let [{:keys [tag data]} (first lines)]
                  (if (contains? #{"CONT" "CONC"} tag)
                    (recur (update-in line [:data] str
                                      (when (= "CONT" tag) "\n") data)
                           (rest lines))
                    (cons line (gedcom-line-seq lines))))))))


(defn level< [& args]
  (apply < (map :level args)))


(defn parse-record
  "Recursively parse a record and all its sub-records."
  [parent gedcom-lines]
  (let [line (first gedcom-lines)]
    (if (and line (level< parent line))
      (let [tail (drop-while (partial level< line) (rest gedcom-lines))]
        (parse-record (update-in parent [(:tag line)] (fnil conj [])
                                 (parse-record line (rest gedcom-lines)))
                      tail))
      parent)))


(defn record-seq
  "Parse a GEDCOM record from a sequence returning a lazy-seq of hashes."
  [gedcom-lines]
  (lazy-seq (if (seq (rest gedcom-lines))
              (let [[head tail] (split-with (comp pos? :level) (rest gedcom-lines))]
                (cons (parse-record (first gedcom-lines) head)
                      (record-seq tail)))
              (take 1 gedcom-lines))))


(defn reader
  "Takes an encoding (from the CHAR tag) and returns a reader
   that reads the GEDCOM file in the proper encoding."
  [encoding in]
  (let [encoding (case (and encoding (.toLowerCase encoding))
                   "ansel"   "ANSEL"
                   "ansi"    "Windows-1252"
                   "cp1252"  "Windows-1252"
                   "unicode" "UTF-16"
                   "utf-8"   "UTF-8"
                   "utf8"    "UTF-8"
                   nil)]
    (if encoding
      (io/reader (InputStreamReader. in encoding))
      (io/reader in))))


(defn ^:private parse-records*
  "Parses GEDCOM records from a file or reader, returning a seq of records.
   Takes an optional encoding."
  [in & [encoding]]
  (->> in
       io/input-stream
       BOMInputStream.
       (reader encoding)
       line-seq
       (map gedcom-line)
       gedcom-line-seq
       record-seq))


(defn parse-records
  "Parses GEDCOM record from a file or reader, returning a seq of records."
  [in]
  (let [records (parse-records* in)
        head (first records)]
    (if-let [encoding (and (= "HEAD" (:tag head))
                           (-> head (get "CHAR") first :data))]
      (parse-records* in encoding)
      records)))


(defn parse
  "Parses GEDCOM records from a file or reader, returning map of labels/tags to records."
  ([in]
   (parse in identity))
  ([in post-process]
   (reduce (fn [records record]
             (assoc records (or (:label record) (:tag record))
                    (post-process record)))
           {}
           (parse-records in))))
