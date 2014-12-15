(ns osmtest.core)

(load "osm_parser")
(load "rest_handler")


; TODO read Data File (and maybe pictures)

(def startPoint [ "5.34977" "50.9348"])

(def xml (parseXml (request "5.3496100" "50.9344600" "5.3517900" "50.9358800")))


; nodes of file
(def nodes (childsByTag xml :node))

; nodes of file
(def nodes (childsByTag xml :node))

; ways of file
(def ways (childsByTag xml :way))

(childsByTag (first ways) :nd )

(circle? (last (take 5 ways)))

(def nodesByMyWay (nodesByWayCurry nodes))



(nodesByMyWay (first ways))


 (getNearestAreaToPoint ways nodes startPoint)


(defn wayHasName?
  "example condition function for checkWay
  @author sören"
  [way]
  (checkWay way (fn[tag] (= "name" (get ( get tag :attrs ) :k)))))

 (filter #(and (circle? %) (wayHasName? %)) ways)


(defn isSwimmmingSport?
  "example condition function for checkWay
  @author sören"
  [way]
  (checkWay way (fn[tag] (and (= "sport" (get ( get tag :attrs ) :k)) (= "swimming" (get ( get tag :attrs ) :v))))))


(map nodesByMyWay (filter #(and (isSwimmmingSport? %)) ways) )

(map parseNodeToCoord  (first (map nodesByMyWay (filter #(and (isSwimmmingSport? %)) ways) ) ))

(map wayTags ways)


(def fw (last (take 3 ways)))

(wayTags fw)
(wayCoords nodes fw)
; TODO parse result of checkWay-functions to xml structure for kml resuklt file



