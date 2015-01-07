(ns osmtest.core)

(load "osm_parser")
(load "rest_handler")
(load "kml_export_service")
(load "utility")

; TODO read Data File (and maybe pictures)

(def rules (osmtest.utility/read-input "resources/Data.txt"))

(defn doLogic
  "takes coords of startpoint and add/subtract 0.0001 to the latitude and longitude to get
  a bounding box arround it. by using this bb, a request get all ways and nodes in this
  bb. this ways get filtert (see comment) and the coords of the most nearst way to the startPoint will be returned.
  if there is no fitting way in this bb, this function will be called recursived with a bigger bb (by multiply the incDec by 2)."
  ([startPoint incDec]
   (let [ lat (last (:coord (last startPoint)))
          lon (first  (:coord (last startPoint)))
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
       (write-kml (str "out/" (first startPoint))  (wayCoords nodes (getNearestAreaToPoint ways nodes [lat lon]))))))
  ([startPoint]
   (let [ incDec 0.0001 ]
      (doLogic startPoint incDec))))


(map doLogic rules)


; TODO map result of checkWay-functions to xml structure for kml resuklt file










;;
;; ab hier nur noch beispiele für aufrufe!!!
;;
;;

(def startPoint [ 5.34977 50.9348])
;(doLogic startPoint)
(def decFactor 0.99999)
(def incFactor 1.00001)

(def xml (parseXml (request (* decFactor (first startPoint)) (* decFactor (last startPoint)) (* incFactor (first startPoint)) (* incFactor (last startPoint)))))


; nodes of file
(def nodes (childsByTag xml :node))

; ways of file
(def ways (childsByTag xml :way))
(first ways)

(map #(:ref (:attrs %)) (childsByTag (first ways) :nd ))

(circle? (last (take 5 ways)))

(def nodesByMyWay (nodesByWayCurry nodes))
nodesByMyWay

(last (take 5 ways))
(nodesByMyWay (last (take 5 ways)))
(childsByTag (last (take 5 ways)) :nd)


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

(write-kml "0011Test2" (map parseNodeToCoord  (first (map nodesByMyWay (filter #(and (isSwimmmingSport? %)) ways) ) )))

(map wayTags ways)


(def fw (last (take 3 ways)))

(wayTags fw)
(wayCoords nodes fw)

;; main function
(defn -main [& args]
  (do(println "hello world")))






