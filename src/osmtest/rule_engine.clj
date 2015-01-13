(ns osmtest.rule_engine
  (require [osmtest.osm_parser :as osm]
           [osmtest.file_reader :as reader])
  )

(def polyRules (-> (reader/directory "./rules/poly-rules")
               reader/only-visible
               reader/only-files
               reader/file-path
               reader/slurp-files
               reader/to-clj
               reader/add-poly-rules))

(def rules (-> (reader/directory "./rules/space-usage-rules")
               reader/only-visible
               reader/only-files
               reader/file-path
               reader/slurp-files
               reader/to-clj
               reader/add-rules))

(defn rule-on-way [attributes way]
  (fn [rule]
  (let [tagName (:locationTag rule)
        tagValue (:locationValue rule)]
    (if (and (or
              (and (not (clojure.string/blank? tagValue )) (osm/checkWay way (fn[tag] (and (= tagName (:k (:attrs tag ))) (= tagValue (:v (:attrs tag)))))))
              (and (clojure.string/blank? tagValue)  (osm/checkWay way (fn[tag] (= tagName (:k (:attrs tag )))))))
             (= (:attributeValue rule) (get attributes (:attributeTag rule))))
        (:points rule)
      0))))

(defn getRanking [attributes way]
  (let [ruleWay (rule-on-way attributes way)]
    (reduce + 1 (map ruleWay rules))))
