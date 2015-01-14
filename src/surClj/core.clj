(ns surClj.core
  (require [surClj.osm_parser :as osm]
           [surClj.rule_engine :as rule_engine]
           [surClj.rest_handler :as rest_handler]
           [surClj.kml_export_service :as kml_export_service]
           [surClj.utility :as utility]
           [surClj.poly :as poly]
           [surClj.space_finder :as space]
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
          borderDistance 10.0
          minDistance  (apply min (map #(poly/point-to-polygon (:coord (last startPoint)) (osm/convertStringCoords (osm/wayCoords nodes %))) areas))
          switchCoords (fn[coords] (map #(vector (last %1) (first %1)) coords))]
     (if (< minDistance borderDistance)
       (let
         [ attrs       (:rules (last startPoint))
           ranks       (let[ranking_and_distance (fn[way]
                                                    (/ (rule_engine/getRanking attrs way)
                                                           (inc (poly/point-to-polygon (:coord (last startPoint)) (osm/convertStringCoords (osm/wayCoords nodes way))))))]
                            (map vector (map #(ranking_and_distance %) areas) areas))]
         (if (empty? areas)
           (recur startPoint (* 2 incDec))
           (kml_export_service/write-kml (str "out/" (first startPoint))
                                          (switchCoords(osm/wayCoords nodes  (second(first(sort-by first > ranks))))))))
        (let
           [space (switchCoords (space/getVisibleSpace (:coord (last startPoint)) incDec ways nodes))]
           (kml_export_service/write-kml (str "out/" (first startPoint)) space)))))
  ([startPoint]
   (let [ incDec 0.002 ]
      (doLogic startPoint incDec))))

(defn -main [& args]
    (dorun (map println (surClj.utility/read-input "./locations/Data.txt"))))
