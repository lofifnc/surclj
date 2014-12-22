(ns osmtest.result-parser
  (require [clojure.string :as str]
           [ clojure.java.io :as io]))

(def path "/home/mario/Dokumente/Master/3.Semester/Clojure/Projekt/testdaten-sur-20102014/Dataset_20102014/")


(for [x (range 1 9)] (let [file1 (slurp (str path "000" x ".truth.kml"))
                           file2 (slurp (str "out/000" x ".kml"))
                           res1 (str/split (str/replace ((str/split file1 #"coordinates" ) 1) #"\>|\<|\r" "") #"\n")
                           res2 (str/split (str/replace ((str/split file2 #"coordinates" ) 1) #"\>|\<|\r" "") #"\n")]
                       (= res1 res2)))


(for [x (range 10 96)] (let [file1 (slurp (str path "00" x ".truth.kml"))
                           file2 (slurp (str "out/00" x ".kml"))
                           res1 (str/split (str/replace ((str/split file1 #"coordinates" ) 1) #"\>|\<|\r" "") #"\n")
                           res2 (str/split (str/replace ((str/split file2 #"coordinates" ) 1) #"\>|\<|\r" "") #"\n")]
                       (= res1 res2)))

