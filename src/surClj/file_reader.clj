(ns surClj.file_reader
  (:require [clojure.java.io :as io]))

(defn directory
  "Directory content"
  [dirpath]
  (file-seq (io/file dirpath)))

(defn only-files
  "filter out directories"
  [files]
  (filter #(.isFile %) files))

(defn only-visible
  "filter out hidden files"
  [files]
  (filter #(not (.isHidden %)) files))

(defn file-path
  "get the path of each file in files"
  [files]
  (map #(.getPath %) files))

(defn load-files
    "load each *.clj file in files
    and evaluate the set of forms contained in the file."
  [files]
  (map #(load-file %) files))

(defn slurp-files
  "slurp the content of each file in files"
  [files]
  (map #(slurp %) files))

(defn to-clj
    "convert the file contents to clojure structures"
  [files]
  (map #(read-string %) files))

(defn add-rules
  "add each space-usage-rule to an array"
  [files]
  (flatten files))

(defn add-poly-rules
  "add each polyl-rule to a map"
  [files]
  (map #(conj {} %) files))
