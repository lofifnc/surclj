(ns osmtest.core)

(load "osm_parser")


; TODO read Data File (and maybe pictures)

; TODO request

; TODO here use response instead of fix file
(def xml (parseXml "resources/schwimmbad.osm"))

; nodes of file
(def nodes (childsByTag xml :node))

; ways of file
(def ways (childsByTag xml :way))

(childsByTag (first ways) :nd )

(circle? (last (take 3 ways)))

(def nodesByMyWay (nodesByWayCurry nodes))

(nodesByMyWay (first ways))

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



