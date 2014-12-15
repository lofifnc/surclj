(ns osmtest.core
   (require [clojure.xml :as xml]
          [clojure.zip :as zip]))

(defn parseXml
  "parse an incoming string into a sequence by xml structure"
  [str]
  (xml/parse str))


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



(defn nodesByWay
  "give a sequence of nodeSet that are used in the given way
  @author sören"
  [ nodeSet way ]
  ((nodesByWayCurry nodeSet) way))


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
  (map parseNodeToCoord (nodesByWay nodeSet fw)))
