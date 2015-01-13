(ns osmtest.space_finder
    (require [osmtest.rest_handler :as rest]
           [osmtest.osm_parser :as osm] 
           [osmtest.poly :as poly]
           [geo [geohash :as geohash] [jts :as jts] [spatial :as spatial] [poly :as pol]]
  )
)

(defn isVisibleNode [start node edges]
  (not(some #(and (pol/edges-intersect? [start node] %1) (not(or (= node (first %1)) (= node (last %1))))) edges))                 
)

(defn edgesOfWay [way nodes]
  (map #(list (vector (java.lang.Double/parseDouble(last(first %1))) (java.lang.Double/parseDouble(first(first %1))))
           (vector (java.lang.Double/parseDouble(last(last %1))) (java.lang.Double/parseDouble(first(last %1))) ))
       (partition 2 1(osm/wayCoords nodes way)))
)

(defn nodeInside [node startPoint incDec]
  (let [nodKoord (osm/parseNodeToCoordDouble node)
        nodLat (first nodKoord)
        nodLon (last nodKoord)
        minLat (-  (first startPoint) incDec)
        maxLat (+  (first startPoint) incDec)
        minLon (-  (last startPoint) incDec)
        maxLon (+  (last startPoint) incDec) 
        
        area (list minLat minLon minLat maxLon maxLat maxLon maxLat minLon minLat minLon)
        ]
   (pol/region-contains? nodLat nodLon area))
)

(defn  angle_of [ank1 ank2 hyp]
  (let [sqrt (fn [x] (* x x))]
  (* (Math/acos (/ (+ (sqrt ank1) (sqrt ank2)(- (sqrt hyp)))  (* 2 ank1 ank2)))
  (/ 360 (* 2 Math/PI))))
)

(defn clockAngle [startPoint node incDec]
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


;Funktion liefert Fläche, wenn kein Polygon gefunden werden kann
; ways not areas
(defn getVisibleSpace [startPoint incDec ways nodes]
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


