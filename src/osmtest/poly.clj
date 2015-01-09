(ns osmtest.poly
  (:use [clojure.math.numeric-tower :only (sqrt expt)])
  (:require [geo [geohash :as geohash] [jts :as jts] [spatial :as spatial]]))

(def uptown
  [
   [-76.899979,40.277908]
   [-76.905258,40.288906]
   [-76.906013,40.290821]
   [-76.906357,40.293571]
   [-76.905853,40.295715]
   [-76.902512,40.303967]
   [-76.900864,40.308659]
   [-76.898247,40.310394]
   [-76.896751,40.310566]
   [-76.893913,40.309837]
   [-76.893478,40.301262]
   [-76.890831,40.292564]
   [-76.887260,40.281490]
   [-76.899979,40.277908]
   ])


(defn dot-product [& matrix]
   "Dot product of given matrix"
  {:pre [(apply == (map count matrix))]}
  (apply + (apply map * matrix)))

(defn magnitude [vektor]
  "Magnitude of given vektor"
	(sqrt (reduce + (map  #(expt % 2) vektor))))

(defn distance_meters [a b]
  (spatial/distance (apply spatial/spatial4j-point a)(apply spatial/spatial4j-point b)))

(defn vecsub[v1 v2]
  "Subtracts two vectors"
  (map - v1 v2))

(comment
"Deprecated"
(defn point-to-linesegment[x p1 p2]
	"Distance point to a line-segment"
  (if (or (= x p1) (= x p2)) 0
	(let [r
		(/ (dot-product (vecsub p2 p1) (vecsub x p1))
			(magnitude (vecsub x p1)))]
		(cond
			(< r 0) (magnitude (vecsub x p1))
			(> r 1) (magnitude (vecsub p2 x))
			:else (sqrt (- (expt (magnitude (vecsub x p1)) 2)(* r (expt (magnitude (vecsub p2 p1)) 2))))))))
)

(comment
"Deprecated"
(defn point-to-linesegment[x p1 p2]
	"Distance point to a line-segment"
  (if (or (= x p1) (= x p2)) 0
	(let [r
		(/ (dot-product (vecsub p2 p1) (vecsub x p1))
			(magnitude (vecsub x p1)))]
		(cond
			(< r 0) (distance_meters x p1)
			(> r 1) (distance_meters p2 x)
			:else (sqrt (- (expt (distance_meters x p1) 2)(* r (expt (distance_meters p2 p1) 2))))))))
)



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
               (>= p1_angle 90.0) p1->x
               (>= p2_angle 90.0) p2->x
               :else (* (Math/sin (* p1_angle (/ (* 2 Math/PI) 360))) p1->x))))))



(defn point-to-polygon [x polygon]
	"Distance point to polygon"
	 (apply min (map  #(point-to-linesegment x (first %) (second %))(partition 2 1 polygon))))

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

(def parking [[5.39449440,50.92540920]
[5.39439760,50.92538520]
[5.39428670,50.92554690]
[5.39435600,50.92556580]
[5.39425200,50.92572160]
[5.39421150,50.92573420]
[5.39419710,50.92571940]
[5.39403610,50.92579710]
[5.39412040,50.92587160]
[5.39419200,50.92588620]
[5.39419780,50.92591840]
[5.39418890,50.92596410]
[5.39430740,50.92599180]
[5.39436250,50.92595470]
[5.39438830,50.92597360]
[5.39462890,50.92594930]
[5.39460260,50.92583600]
[5.39456620,50.92583520]
[5.39455300,50.92580050]
[5.39477580,50.92548380]
[5.39449440,50.92540920]])

(point-inside? [-76.905258,40.288906] uptown) ; true

(point-inside? [-73.905258,49.288906] uptown) ; false

(distance_meters [51.477500 -0.461388] [33.942495 -118.408067])

(point-to-polygon [-76.905258,40.288906] uptown) ; 0

(point-to-linesegment [-76.905258,40.288906] [-76.906357 40.293571] [-76.905853 40.295715])

(point-to-polygon [-73.905258,49.288906] uptown) ; 417531.32415376697

(point-inside? [5.39412, 50.9259] parking)

(point-to-polygon [5.39412, 50.9259] parking)

