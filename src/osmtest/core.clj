(ns osmtest.core)

(load "osm_parser")
(load "rest_handler")


; TODO read Data File (and maybe pictures)

(def startPoint [ 5.34977 50.9348])

(defn doLogic
  "takes coords of startpoint and add/subtract 0.0001 to the latitude and longitude to get
  a bounding box arround it. by using this bb, a request get all ways and nodes in this
  bb. this ways get filtert (see comment) and the coords of the most nearst way to the startPoint will be returned.
  if there is no fitting way in this bb, this function will be called recursived with a bigger bb (by multiply the incDec by 2)."
  ([startPoint incDec]
   (let [ lat (first startPoint)
          lon (last startPoint)
          xml         (parseXml (request (-  lat incDec)
                                         (-  lon incDec)
                                         (+  lat incDec)
                                         (+  lon incDec)))
          nodes       (childsByTag xml :node)
          ways        (filter #(and (circle? %) ;; hier könnten unsere Regeln stehen
                                    ) (childsByTag xml :way))
          nodesByWay  (nodesByWayCurry nodes)]
     (if (empty? ways)
       (recur startPoint (* 2 incDec))
       (wayCoords nodes (getNearestAreaToPoint ways nodes startPoint)))))
  ([startPoint]
   (let [ incDec 0.0001 ]
      (doLogic startPoint incDec))))

(doLogic startPoint)

; TODO map result of checkWay-functions to xml structure for kml resuklt file





;;
;; ab hier nur noch beispiele für aufrufe!!!
;;
;;

(def decFactor 0.99999)
(def incFactor 1.00001)

(def xml (parseXml (request (* decFactor (first startPoint)) (* decFactor (last startPoint)) (* incFactor (first startPoint)) (* incFactor (last startPoint)))))


; nodes of file
(def nodes (childsByTag xml :node))

; ways of file
(def ways (childsByTag xml :way))

(childsByTag (first ways) :nd )

(circle? (last (take 5 ways)))

(def nodesByMyWay (nodesByWayCurry nodes))


(last (take 5 ways))
(nodesByMyWay (last (take 5 ways)))


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








