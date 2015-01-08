(ns osmtest.rule_engine
   (require [osmtest.osm_parser :as osm]))


(def rules [
  { :attributeTag "smoking"
    :attributevalue "no"
    :locationTag "building"
    :locationValue "yes"
    :points 5}

  { :attributeTag "smoking"
    :attributevalue "no"
    :locationTag "amenity"
    :locationValue "parking"
    :points 1}


            ])

(def attrs {"noise" "no", "access:motorvehicles" "no", "littering" "no", "dog_waste" "no"})



(defn rule-on-way [attributes way]
  (fn [rule]
  (let [wayHasTag? (fn[way tag]
                    (osm/checkWay way (fn[tag] (= tag (:k (:attrs tag ))))))]
    (if (and (wayHasTag? way (:locationTag rule)) (get attributes (:attributeTag rule)))
      (:points rule)
      0))))



(defn getRanking [attributes way]
  (let [ruleWay (rule-on-way attributes way)]
   (reduce + 0 (map ruleWay rules))))


