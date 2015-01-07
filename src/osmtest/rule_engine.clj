(ns osmtest.rule_engine)

(def rules [
  { :attributeTag "smoking"
    :attributevalue "no"
    :locationTag "building"
    :locationValue "yes"
    :points 2}

  { :attributeTag "smoking"
    :attributevalue "no"
    :locationTag "amenity"
    :locationValue "parking"
    :points 0}


            ])



(defn getRanking [attributes way]
  (
