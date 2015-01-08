(ns osmtest.core
  (require [osmtest.osm_parser :as osm]
           [osmtest.rule_engine :as rule_engine]
           [osmtest.utility :as util]))


(load "rest_handler")
(load "kml_export_service")

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
          xml         (osm/parseXml (request (-  lat incDec)
                                         (-  lon incDec)
                                         (+  lat incDec)
                                         (+  lon incDec)))
          nodes       (osm/childsByTag xml :node)
          ways        (filter #(and (osm/circle? %) ;; hier könnten unsere Regeln stehen
                                    ) (osm/childsByTag xml :way))
          nodesByWay  (osm/nodesByWayCurry nodes)]
     (if (empty? ways)
       (recur startPoint (* 2 incDec))
       (write-kml (str "out/" (first startPoint))  (osm/wayCoords nodes (osm/getNearestAreaToPoint ways nodes [lat lon]))))))
  ([startPoint]
   (let [ incDec 0.0001 ]
      (doLogic startPoint incDec))))


(map doLogic rules)


; TODO map result of checkWay-functions to xml structure for kml resuklt file










;;
;; ab hier nur noch beispiele für aufrufe!!!
;;
;;


(def data_txt (util/read-input "resources/Data.txt"))

(def ID "0006")

(def startPoint (:coord (get data_txt ID)))
;(doLogic startPoint)
(def decFactor 0.99999)
(def incFactor 1.00001)

(def xml (osm/parseXml (request (* decFactor (last startPoint)) (* decFactor (first startPoint)) (* incFactor (last startPoint)) (* incFactor (first startPoint)))))


; nodes of file
(def nodes (osm/childsByTag xml :node))

; ways of file
(def ways (osm/childsByTag xml :way))
(first ways)

(map #(:ref (:attrs %)) (osm/childsByTag (first ways) :nd ))

(osm/circle? (last (take 5 ways)))

(def nodesByMyWay (osm/nodesByWayCurry nodes))
nodesByMyWay

(last (take 1 ways))
(nodesByMyWay (last (take 5 ways)))
(osm/childsByTag (last (take 5 ways)) :nd)


 (osm/getNearestAreaToPoint ways nodes startPoint)


(defn wayHasName?
  "example condition function for checkWay
  @author sören"
  [way]
  (osm/checkWay way (fn[tag] (= "name" (:k (:attrs tag ))))))

 (filter #(and (osm/circle? %) (wayHasName? %)) ways)


(defn isSwimmmingSport?
  "example condition function for checkWay
  @author sören"
  [way]
  (osm/checkWay way (fn[tag] (and (= "sport" (get ( get tag :attrs ) :k)) (= "swimming" (get ( get tag :attrs ) :v))))))

(defn wayHasTag? [way tag]
    (osm/checkWay way (fn[tag] (= tag (:k (:attrs tag ))))))



(map nodesByMyWay (filter #(and (isSwimmmingSport? %)) ways) )

(write-kml "0011Test2" (map osm/parseNodeToCoord  (first (map nodesByMyWay (filter #(and (isSwimmmingSport? %)) ways) ) )))

(map osm/wayTags ways)

(last (take 6 ways))

(count ways)

(def attrs (util/rules-by-id ID (util/read-input "resources/Data.txt")))

attrs

(def ranks(map vector (map #(rule_engine/getRanking attrs %) ways) ways))

(sort-by first > ranks)


;; main function
(defn -main [& args]
  (do(println "hello world")))

