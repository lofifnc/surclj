(ns osmtest.poly
  (:require [geo [geohash :as geohash] [jts :as jts] [spatial :as spatial]]))

 (defn distance_meters [a b]
  "computes the distance between to points in meters"
  (spatial/distance (apply spatial/spatial4j-point a)(apply spatial/spatial4j-point b)))

(defn point-to-linesegment[x p1 p2]
  (let [
        p1->p2 (distance_meters p1 p2)
        p1->x (distance_meters p1 x)
        p2->x (distance_meters p2 x)]
    (cond
     (some #(= 0.0 %) [ p2->x p1->x]) 0
     (= 0.0 p1->p2) p1->x
     :else (let [ sqrt (fn[x] (* x x))
                  angle_of (fn[ank1 ank2 hyp]
                              (* (Math/acos (/ ( + (sqrt ank1)
                                                (sqrt ank2)
                                                (-  (sqrt hyp)))
                                            (* 2 ank1 ank2)))
                                 (/ 360 (* 2 Math/PI))))
                  p1_angle (angle_of p1->x p1->p2 p2->x)
                  p2_angle (angle_of p1->p2 p2->x p1->x)]
             (cond
               (some #(Double/isNaN %) [p1_angle p2_angle]) (apply min [p1->x p2->x])
               (>= p1_angle 90.0) p1->x
               (>= p2_angle 90.0) p2->x
               :else (* (Math/sin (* p1_angle (/ (* 2 Math/PI) 360))) p1->x))))))



(defn- crossing-number
  "Determine crossing number for given point and segment of a polygon.
   See http://geomalgorithms.com/a03-_inclusion.html"
  [[px py] [[x1 y1] [x2 y2]]]
  (if (or (and (<= y1 py) (> y2 py))
          (and (> y1 py) (<= y2 py)))
    (let [vt (/ (- py y1) (- y2 y1))]
      (if (< px (+ x1 (* vt (- x2 x1))))
        1 0))
    0))

(defn point-inside?
  "Is point inside the given polygon?"
  [point polygon]
  (odd? (reduce + (map #(crossing-number point %)(partition 2 1 polygon)))))


(defn point-to-polygon [x polygon]
	"Distance point to polygon"
	 (apply min (map  #(point-to-linesegment x (first %) (second %))(partition 2 1 polygon))))



