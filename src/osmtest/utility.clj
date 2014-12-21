(ns osmtest.utilily
  (:require [clojure.string :as str]))

(defn assoc-line[m kv]
  (let [k (first kv) v (rest kv)]
  (if (contains? m k)
    (assoc m k {:coord [(read-string (first v)) (read-string (second v))]
                :rules (conj (:rules (m k)) (last v))})
    (assoc m k {:coord [(read-string (first v)) (read-string (second v))]
                :rules [(last v)]}))))

(defn read-input[file]
    (reduce #(assoc-line %1 (str/split %2 #", ")) {} (rest (str/split (slurp file) #"\n"))))


(read-input "resources/Data.txt")
