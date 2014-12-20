(ns osmtest.poly
  (:use [clojure.math.numeric-tower :only (sqrt expt)]))

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
   "dot product of given matrix"
  {:pre [(apply == (map count matrix))]}
  (apply + (apply map * matrix)))

(defn magnitude [vektor]
  "Magnitude of given vektor"
	(sqrt (reduce + (map  #(expt % 2) vektor))))

(defn vecsub[v1 v2]
  "Subtracts two vectors"
  (map - v1 v2))

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

(defn point-to-polygon [x polygon]
	"Distance point to polygon"
	(apply min (map #(point-to-linesegment x (first %) (second %))(partition 2 1 polygon))))

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


(point-inside? [-76.905258,40.288906] uptown) ; true

(point-inside? [-73.905258,49.288906] uptown) ; false

(point-to-polygon [-76.905258,40.288906] uptown) ; 0

(point-to-polygon [-73.905258,49.288906] uptown) ; 9.463389393646754



