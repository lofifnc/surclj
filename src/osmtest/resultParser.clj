(ns osmtest.result-parser
  (require [clojure.string :as str]
           [ clojure.java.io :as io]))

(def path "/home/seegy/Downloads/Dataset_20102014/")


(defn compareLists
  [a b]
  (let [in? (fn[seq elm] (some #(= elm %) seq))]
   (and (reduce #(and %1 %2) (map #(in? b %) a))
        (reduce #(and %1 %2) (map #(in? a %) b)))))

(for [x (range 1 9)] (let [file1 (slurp (str path "000" x ".truth.kml"))
                           file2 (slurp (str "out/000" x ".kml"))
                           res1 (str/split (str/replace ((str/split file1 #"coordinates" ) 1) #"\>|\<|\r" "") #"\n")
                           res2 (str/split (str/replace ((str/split file2 #"coordinates" ) 1) #"\>|\<|\r" "") #"\n")]
                       (vector (str "000" x) (map compareLists [res1] [res2]))))


(for [x (range 10 96)] (let [file1 (slurp (str path "00" x ".truth.kml"))
                           file2 (slurp (str "out/00" x ".kml"))
                           res1 (str/split (str/replace ((str/split file1 #"coordinates" ) 1) #"\>|\<|\r" "") #"\n")
                           res2 (str/split (str/replace ((str/split file2 #"coordinates" ) 1) #"\>|\<|\r" "") #"\n")]
                       (vector (str "00" x) (map compareLists [res1] [res2]))))

