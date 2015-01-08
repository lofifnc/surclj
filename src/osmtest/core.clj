(ns osmtest.core
  (require [osmtest.osm_parser :as osm]
           [osmtest.rule_engine :as rule_engine]
           [osmtest.rest_handler :as rest_handler]
           [osmtest.kml_export_service :as kml_export_service]
           [osmtest.utility :as utility])
  (:gen-class :main true))

; (load "rest_handler")
; (load "kml_export_service")
; (load "utility")

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
          xml         (osm/parseXml (rest_handler/request (-  lat incDec)
                                         (-  lon incDec)
                                         (+  lat incDec)
                                         (+  lon incDec)))
          nodes       (osm/childsByTag xml :node)
          ways        (filter #(and (osm/circle? %) ;; hier könnten unsere Regeln stehen
                                    ) (osm/childsByTag xml :way))
          nodesByWay  (osm/nodesByWayCurry nodes)]
     (if (empty? ways)
       (recur startPoint (* 2 incDec))
       (kml_export_service/write-kml (str "out/" (first startPoint))  (osm/wayCoords nodes (osm/getNearestAreaToPoint ways nodes [lat lon]))))))
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

(def xml (osm/parseXml (rest_handler/request (* decFactor (first startPoint)) (* decFactor (last startPoint)) (* incFactor (first startPoint)) (* incFactor (last startPoint)))))


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


(map nodesByMyWay (filter #(and (isSwimmmingSport? %)) ways) )

(kml_export_service/write-kml "0011Test2" (map osm/parseNodeToCoord  (first (map nodesByMyWay (filter #(and (isSwimmmingSport? %)) ways) ) )))

(map osm/wayTags ways)


(def fw (last (take 11 ways)))

(osm/wayTags fw)
(osm/wayCoords nodes fw)

(def attrs {"noise" "no", "access:motorvehicles" "no", "littering" "no", "dog_waste" "no"})
(rule_engine/getRanking attrs fw)


;; main function
; (defn -main [& args]
;   (do(println "hello world")))
(defn -main [& args]
  (println "Welcome to my project! These are your args:" args))
