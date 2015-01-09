(ns osmtest.osm_parser
   (require [clojure.xml :as xml]
            [clojure.zip :as zip]))


(defn parseXml
  "parse an incoming string into a sequence by xml structure"
  [s]
  (xml/parse (java.io.ByteArrayInputStream. (.getBytes s "UTF-8"))))



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
  "a curry-ing function to temporary save the set of nodes in the function 'nodesByWay'"
  [nodeSet]
  (fn [way]
    (let [getNode (fn [nd nodes]
          (if (= (get (get nd :attrs) :ref) (get (get (first nodes) :attrs) :id))
            (first nodes)
            (recur nd (rest nodes))))
          getNodeBySet (fn[nd]
                         (getNode nd nodeSet))]
       (map getNodeBySet (childsByTag way :nd)))))


(defn nodesByWay
  "give a sequence of nodes from the nodeSet that are used in the given way in the same oder as the way refernes
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
  "delivers by sets of ways and nodes the way nearest to the start point"
  [ways nodeSet startPoint ]
  (let
    [distanceBetweenStartAnd (fn[node]
          (let [[x,y] (parseNodeToCoord node)]
             (Math/sqrt (+ (Math/pow (- (read-string x)  (get startPoint 0)) 2)
                    (Math/pow (- (read-string y)  (get startPoint 1)) 2)))))
      minWayDistance (fn[way] (vector (first (sort > (map distanceBetweenStartAnd (nodesByWay nodeSet way)))) way))]
    (last (first (sort-by first < (map minWayDistance  ways))))))


(defn convertStringCoords[nodes]
  (let [ convert (fn[[a b]]
                   (vector (read-string a) (read-string b)))]
    (map convert nodes)))
