(ns osmtest.core
  (require [osmtest.osm_parser :as osm]
           [osmtest.rule_engine :as rule_engine]
           [osmtest.rest_handler :as rest_handler]
           [osmtest.kml_export_service :as kml_export_service]
           [osmtest.utility :as utility]
           [osmtest.poly :as poly]
           [osmtest.space_finder :as space]
           [geo [geohash :as geohash] [jts :as jts] [spatial :as spatial] [poly :as pol]])
  (:gen-class :main true))

(defn doLogic
  "takes coords of startpoint and add/subtract 0.001 to the latitude and longitude to get
  a bounding box arround it. by using this bb, a request get all ways and nodes in this
  bb. this ways get filtert (see comment) and the coords of the most nearst way to the startPoint will be returned.
  if there is no fitting way in this bb, this function will be called recursived with a bigger bb (by multiply the incDec by 2)."
  ([startPoint incDec]
   (let
     [ lat (last (:coord (last startPoint)))
          lon (first  (:coord (last startPoint)))
          xml         (osm/parseXml (rest_handler/request
                                         (-  lat incDec)
                                         (-  lon incDec)
                                         (+  lat incDec)
                                         (+  lon incDec)))
          nodes       (osm/childsByTag xml :node)
          ways        (osm/childsByTag xml :way)
          areas       (filter #(osm/circle? %) ways)
          attrs       (:rules (last startPoint))
          borderDistance 10.0
          minDistance  (apply min (map #(poly/point-to-polygon (:coord (last startPoint)) (osm/convertStringCoords (osm/wayCoords nodes %))) areas))
          umgebPol     (filter #(poly/point-inside? (:coord (last startPoint)) (osm/convertStringCoords (osm/wayCoords nodes %))) areas)
          umgebPolNahe (first(sort-by #(< (first %)) (map #(list (poly/point-to-polygon (:coord (last startPoint)) (osm/convertStringCoords (osm/wayCoords nodes %)))
                                                               %)      umgebPol)))
          ;;kann hier was passieren, wenn Liste leer ist?? auch nochmal auf Zeile 51 gucken
          ;;kann nicht mehr denken, ist zu spät. Bisher ist halt kein fehler aufgetreten
          
          switchCoords (fn[coords] (map #(vector (last %1) (first %1)) coords))]
     (if (< minDistance borderDistance)
       (let
         [ 
           ranks       (let[ranking_and_distance (fn[way]
                                                    (/ (rule_engine/getRanking attrs way)
                                                           (inc (poly/point-to-polygon (:coord (last startPoint)) (osm/convertStringCoords (osm/wayCoords nodes way))))))]
                            (map vector (map #(ranking_and_distance %) areas) areas))]

         (if (empty? areas)
           (recur startPoint (* 2 incDec))
           (kml_export_service/write-kml (str "out/P" (first startPoint))
                                          (switchCoords(osm/wayCoords nodes  (second(first(sort-by first > ranks))))))))
        (if (or (= minDistance (first umgebPolNahe)) (> (rule_engine/getRanking attrs (last umgebPolNahe)) 1) ) ;or oder and
          (let     
            [space   (switchCoords (osm/convertStringCoords (osm/wayCoords nodes (last umgebPolNahe))))]
            (kml_export_service/write-kml (str "out/I" (first startPoint)) space))
          (let
            [space (switchCoords (space/getVisibleSpace (:coord (last startPoint)) incDec ways nodes))]
            (kml_export_service/write-kml (str "out/S" (first startPoint)) space))))))
  ([startPoint]
   (let [ incDec 0.002 ]
      (doLogic startPoint incDec))))

(defn -main [& args]
    (dorun (map doLogic (osmtest.utility/read-input "./resources/Data.txt"))))


