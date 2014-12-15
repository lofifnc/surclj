(ns osmtest.core
  (:require [clojure.java.io :as io]))


(defn request [left bottom right top]
  (let [uri (str " http://api.openstreetmap.org/api/0.6/map?bbox=" left "," bottom "," right "," top)]
    (slurp uri)))
