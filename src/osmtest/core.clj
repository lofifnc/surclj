(ns osmtest.core
 (require [clojure.xml :as xml]
          [clojure.zip :as zip]))



; TODO read Data File (and maybe pictures)

; TODO request

; TODO here use response instead of fix file
(def xml (xml/parse  "resources/schwimmbad.osm"))





(defn childsByTag
  "give a sequence of xml child item with the given tag from the given root xml"
  [tag xml]
  (filter #(= tag (get % :tag)) (get xml :content)))

; nodes of file
(def nodes (childsByTag :node xml))

; ways of file
(def ways (childsByTag :way xml))






(defn circle? [way]
  "is this way a closed circle?"
  (let [nodes (childsByTag :nd way)]
        (= (get (get (first nodes) :attrs) :ref) (get (get (last nodes) :attrs) :ref))))

(circle? (last (take 5 ways)))






(defn nodesByWay [way]
  "give a sequence of nodes that are used in the given way"
  (let [nodeInWayRefs (fn [node refs]
          (if (= (get (get node :attrs) :id) (get (get (first refs) :attrs) :ref))
            true
            (if (empty? (rest refs))
              false
              (recur node (rest refs)))))]
    (filter #(nodeInWayRefs % (childsByTag :nd way)) nodes)))

(nodesByWay (first ways))





(defn checkWay
  "check tags of given way by condition function (fn[tag])"
  [way condFn]
  (let
    [checkTag (fn[tags]
                (if (condFn (first tags))
                    true
                    (if (empty? (rest tags))
                      false
                      (recur (rest tags)))))]
    (checkTag (childsByTag :tag way))))





(defn wayHasName?
  "example condition function for checkWay"
  [way]
  (checkWay way (fn[tag] (= "name" (get ( get tag :attrs ) :k)))))

(map nodesByWay (filter #(and (circle? %) (wayHasName? %)) ways))






(defn isSwimmmingSport?
  "example condition function for checkWay"
  [way]
  (checkWay way (fn[tag] (and (= "sport" (get ( get tag :attrs ) :k)) (= "swimming" (get ( get tag :attrs ) :v))))))


(map nodesByWay (filter #(and (isSwimmmingSport? %)) ways))


; TODO parse result of checkWay-functions to xml structure for kml resuklt file



