(ns surClj.core
  (require [surClj.osm_parser :as osm]
           [surClj.rule_engine :as rule_engine]
           [surClj.rest_handler :as rest_handler]
           [surClj.kml_export_service :as kml_export_service]
           [surClj.utility :as utility]
           [surClj.poly :as poly]
           [surClj.space_finder :as space]
           [clojure.java.io :as io]
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
          umgebPolNahe (first(sort-by first (map #(list (poly/point-to-polygon (:coord (last startPoint)) (osm/convertStringCoords (osm/wayCoords nodes %)))
                                                      %)      umgebPol)))

          switchCoords (fn[coords] (map #(vector (last %1) (first %1)) coords))]
     (if (< minDistance borderDistance)
       (let
         [
           ranks       (let[ranking_and_distance (fn[way]
                                                    (/ (* (rule_engine/getRanking attrs way) 5)
                                                           (inc (poly/point-to-polygon (:coord (last startPoint)) (osm/convertStringCoords (osm/wayCoords nodes way))))))]
                            (map vector (map #(ranking_and_distance %) areas) areas))]

         (if (empty? areas)
           (recur startPoint (* 2 incDec))
           (kml_export_service/write-kml (str "out/" (first startPoint))
                                          (switchCoords(osm/wayCoords nodes  (second(first(sort-by first > ranks))))))))
        (if (and (or (= minDistance (first umgebPolNahe)) (> (rule_engine/getRanking attrs (last umgebPolNahe)) 1) )
                 (< (poly/point-to-polygon-max (:coord (last startPoint)) (osm/convertStringCoords (osm/wayCoords nodes (last umgebPolNahe))) ) 500))
          (let
            [space   (switchCoords (osm/convertStringCoords (osm/wayCoords nodes (last umgebPolNahe))))]
            (kml_export_service/write-kml (str "out/" (first startPoint)) space))
          (let
            [space (switchCoords (space/getVisibleSpace (:coord (last startPoint)) incDec ways nodes))]
            (kml_export_service/write-kml (str "out/" (first startPoint)) space))))))
     ([startPoint]
      (let [ incDec 0.002 ]
         (doLogic startPoint incDec))))

(defn check-file [fpath]
  (if (.exists (io/file fpath)) true false))

(defn run [fpath]
  (dorun (map doLogic (surClj.utility/read-input fpath))))

(defn -main [& args]
  (dorun
    (let [arg1 (first args)]
      (cond
        (string? arg1) (if (check-file arg1) (run arg1) (println (str "File not found <" arg1 ">")))
        :else (println "To start the application run <java -jar path/to/Data.txt>")))))
