(ns osmtest.rule_engine)

(def polyRules 
  { :smoking true
    :swimming true
    :parking true
    :access false
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



