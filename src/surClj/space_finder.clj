(ns surClj.space_finder
    (require [surClj.rest_handler :as rest]
           [surClj.osm_parser :as osm]
           [surClj.poly :as poly]
           [geo [geohash :as geohash] [jts :as jts] [spatial :as spatial] [poly :as pol]]
  )
)
;Methoden zur Berechnung der eines Polygons in eine freie Fläche

(defn isVisibleNode
  "Ein Ray-casting Algorithmus zur Überprüfung, ob der Node direkt mit dem Startpunkt
   verbunden werden kann, ohne einen Weg zu schneiden."
  [start node edges]
  (not (some #(and
                (pol/edges-intersect? [start node] %1)
                (not(or (= node (first %1)) (= node (last %1)))))
         edges)
  )
)

(defn edgesOfWay
  "Liefert für einen Weg die Liste aller Kanten. Jede Kante ist eine Liste, die 2 Vektoren mit den Koordinaten
   der beiden Eckpunkte enthält."
  [way nodes]
  (map #(list
          (vector (java.lang.Double/parseDouble(first(first %1))) (java.lang.Double/parseDouble(last(first %1))))
          (vector (java.lang.Double/parseDouble(first(last  %1))) (java.lang.Double/parseDouble(last(last  %1))))
        )
       (partition 2 1 (osm/wayCoords nodes way)))
)

(defn nodeInside
  "Prüft, ob ein Knoten in dem angedachten Abschnitt liegt."
  [node startPoint incDec]
  (let [nodeKoord (osm/parseNodeToCoordDouble node)
        nodeLat (first nodeKoord)
        nodeLon (last nodeKoord)
        minLat (-  (first startPoint) incDec)
        maxLat (+  (first startPoint) incDec)
        minLon (-  (last startPoint) incDec)
        maxLon (+  (last startPoint) incDec)

        area (list minLat minLon minLat maxLon maxLat maxLon maxLat minLon minLat minLon)
        ]
   (pol/region-contains? nodeLat nodeLon area))
)

(defn  angle_of
  "Berechnet den Winkel eines Dreicks mit 3 gegeben Seiten. Winkel gegenüber der zuletzt eingegeben
   Seite (hyp)"
  [ank1 ank2 hyp]
  (let [sqrt (fn [x] (* x x))]
  (* (Math/acos (/ (+ (sqrt ank1) (sqrt ank2)(- (sqrt hyp)))  (* 2 ank1 ank2)))
  (/ 360 (* 2 Math/PI))))
)

(defn clockAngle
  "Berechnet den Winkel eines Knotens gegenüber einem normalen Vektor auf dem Startpunkt.
   Der normalen Vektor zeigt Richtung Norden. Winkel im Westen sind kleiner als 180 Grad,
   die im Osten größer als 180 Grad."
  [startPoint node incDec]
  (let [
     highPoint [(+ incDec (first startPoint)) (last startPoint)]
     ank1 (poly/distance_meters startPoint highPoint)
     ank2 (poly/distance_meters startPoint node)
     hyp  (poly/distance_meters node highPoint)
     angle (angle_of ank1 ank2 hyp)
   ]
    (if (< (last node) (last startPoint))
      angle
      (- 360 angle))
    )
)


(defn getVisibleSpace
  "Funktion liefert die leere Fläche im Umkreis des Startpunkt. Die Ecken der
   Fläche sind die Knoten der angrenzenden Wege. (Format Liste von Vektoren)
   Es werden alle Wege benötigt, auch die nicht geschlossenen."
  [startPoint incDec ways nodes]
  (let [ edges (mapcat #(edgesOfWay %1 nodes) ways)
         filteredNodes (filter #(nodeInside %1 startPoint incDec) nodes)
         visNodes (map osm/parseNodeToCoordDouble (filter #(isVisibleNode startPoint (osm/parseNodeToCoordDouble %1) edges) filteredNodes))
         angledNodeList (sort-by #(clockAngle startPoint %1 incDec) visNodes)
        ]
    angledNodeList
   )
)


;Ansatz für konvexes innneres Polygon
;NOCH NICHT VERWENDBAR

(defn isVisibleWay [node1 node2 edges]
  (not(some #(and (pol/edges-intersect? [node1 node2] %1)
                  (not(or (= node1 (first %1)) (= node2 (first %1))
                          (= node1 (last %1)) (= node2 (last %1)))))
       edges))
)

(defn findCycle
    ([angledNodeList edges] (findCycle angledNodeList 2 (list (first angledNodeList)) edges))
    ([angledNodeList int cycle edges]
      (if (> int (count angledNodeList))
             cycle
             (let [next (last (take int angledNodeList))]
               (if (isVisibleWay (last cycle) next edges)
                 (findCycle angledNodeList (+ 1 int) (conj cycle next) edges)
                 (findCycle angledNodeList (+ 1 int) cycle edges))))
     )
)


