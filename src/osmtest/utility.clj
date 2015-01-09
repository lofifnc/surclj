(ns osmtest.utility
  (:require [clojure.string :as str]))

(defn assoc-line[m kv]
  (let [k (first kv)
        v (rest kv)
        split-rule (fn[raw-rule] {(first (str/split raw-rule #"=")) (str/replace (second (str/split raw-rule #"=")) "\"" "") })]
  (if (contains? m k)
    (assoc m k {:coord [(read-string (first v)) (read-string (second v))]
                :rules (conj (:rules (m k)) (split-rule (last v)))})
    (assoc m k {:coord [(read-string (first v)) (read-string (second v))]
                :rules (split-rule (last v))}))))

(defn read-input[file]
    (reduce #(assoc-line %1 (str/split %2 #", ")) {} (rest (str/split (slurp file) #"\n"))))

(defn rules-by-id [id input]
   (:rules (get input id)))




(read-input "resources/Data.txt")

(rules-by-id "0011" (read-input "resources/Data.txt"))
