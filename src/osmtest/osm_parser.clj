(ns osmtest.core
   (require [clojure.xml :as xml]
          [clojure.zip :as zip]))


(defn parseXml
  "parse an incoming string into a sequence by xml structure"
  [s]
      (xml/parse (java.io.ByteArrayInputStream. (.getBytes s))))

(defn parseXmlOld
  "parse an incoming string into a sequence by xml structure"
  [str]
  (zip-str str))


(defn childsByTag
  "give a sequence of xml child item with the given tag from the given root xml
  @author sören"
  [ xml tag ]
  (filter #(= tag (get % :tag)) (get xml :content)))



(defn circle?
  "is this way a closed circle?
  @author sören"
  [ way ]
  (let [nodes (childsByTag way :nd )]
        (= (get (get (first nodes) :attrs) :ref) (get (get (last nodes) :attrs) :ref))))


(defn nodesByWayCurry
  ""
  [nodeSet]
  (fn [way]
    (let [nodeInWayRefs (fn [node refs]
          (if (= (get (get node :attrs) :id) (get (get (first refs) :attrs) :ref))
            true
            (if (empty? (rest refs))
              false
              (recur node (rest refs)))))]
       (filter #(nodeInWayRefs % (childsByTag way :nd)) nodeSet))))


(defn nodesByWay
  "give a sequence of nodeSet that are used in the given way
  @author sören"
  [ nodeSet way ]
  ((nodesByWayCurry nodeSet) way))


(defn checkWay
  "check tags of given way by condition function (fn[tag])
  @author sören"
  [way condFn]
  (let
    [checkTag (fn[tags]
                (if (condFn (first tags))
                    true
                    (if (empty? (rest tags))
                      false
                      (recur (rest tags)))))]
    (checkTag (childsByTag way :tag))))


(defn parseNodeToCoord
  "returns a the coords of a node as vector"
  [node]
  ( vector (get (get node :attrs) :lon) (get ( get node :attrs) :lat )))


(defn wayTags
  "returns a sequence of k-v pairs as the tags of the given way"
  [way]
  (let [filterTag (fn
                    [tag]
                    (get tag :attrs))]
   (map filterTag (childsByTag way :tag))))

(defn wayCoords
  [nodeSet way]
  (map parseNodeToCoord (nodesByWay nodeSet way)))


(defn getNearestAreaToPoint
  [ways nodeSet startPoint ]
  (let
    [distanceBetweenStartAnd (fn[node]
          (let [[x,y] (parseNodeToCoord node)]
             (Math/sqrt (+ (Math/pow (- (read-string x) (read-string (get startPoint 0))) 2)
                    (Math/pow (- (read-string y) (read-string (get startPoint 1))) 2)))))
      minWayDistance (fn[way]
          (vector (first (sort > (map distanceBetweenStartAnd (nodesByWay nodeSet way)))) way))]
    (last (first (sort-by first < (map minWayDistance (filter #(circle? %) ways)))))))
