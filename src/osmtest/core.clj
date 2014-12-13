(ns osmtest.core
 (require [clojure.xml :as xml]
          [clojure.zip :as zip]))




(def xml (xml/parse  "resources/schwimmbad.osm"))


(defn childsByTag [tag xml] (filter #(= tag (get % :tag)) (get xml :content)))

(def nodes (childsByTag :node xml))
(def ways (childsByTag :way xml))


(defn circle? [way]
  (let [nodes (childsByTag :nd way)]
        (= (get (get (first nodes) :attrs) :ref) (get (get (last nodes) :attrs) :ref))))


(circle? (last (take 3 ways)))



(defn nodesByWay [way]
  (let [nodeInWayRefs (fn [node refs]
          (if (= (get (get node :attrs) :id) (get (get (first refs) :attrs) :ref))
            true
            (if (empty? (rest refs))
              false
              (recur node (rest refs)))))]
    (filter #(nodeInWayRefs % (childsByTag :nd way)) nodes)))



(nodesByWay (first ways))



(defn checkWay
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
  [way]
  (checkWay way (fn[tag] (= "name" (get ( get tag :attrs ) :k)))))

(map nodesByWay (filter #(and (circle? %) (wayHasName? %)) ways))


(defn isSwimmmingSport?
  [way]
  (checkWay way (fn[tag] (and (= "sport" (get ( get tag :attrs ) :k)) (= "swimming" (get ( get tag :attrs ) :v))))))


(map nodesByWay (filter #(and (isSwimmmingSport? %)) ways))






