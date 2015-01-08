(ns osmtest.rule_engine
   (require [osmtest.osm_parser :as osm]))


(def polyRules
  { "smoking" true
    "swimming" true
    "parking" true
    "open_fire" true
    "access" false
  }
)

(def rules [
 { :attributeTag "smoking"
   :attributevalue "no"
   :locationTag "building"
   :locationValue "true"
   :points 5}

{ :attributeTag "smoking"
  :attributevalue "no"
  :locationTag "amenity"
  :locationValue "parking"
  :points 1}

 ])


(defn rule-on-way [attributes way]
  (fn [rule]
  (let [tagName (:locationTag rule)]
    (if (and (osm/checkWay way (fn[tag] (= tagName (:k (:attrs tag )))))
             (not (clojure.string/blank? (get attributes (:attributeTag rule)))))
     (:points rule)
      0))))


(defn getRanking [attributes way]
  (let [ruleWay (rule-on-way attributes way)]
    (reduce + 0(map ruleWay rules))))
