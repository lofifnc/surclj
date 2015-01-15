(ns surClj.kml_export_service
  (:require [clojure.xml :as xml]
            [clojure.string :as string])
  )

(defn to-coordinates-str [coords]
  "convert incoming list of vectors to strings"
  (string/join  "\n" (map #(str (string/join "0," %) "0") coords)))

(defn kml-tmpl [filename coords-str]
    "The KML template, img src and coordinates will be parsed dynamicly"
    {:tag :kml, :attrs {:xmlns "http://earth.google.com/kml/2.1"}, :content [
        {:tag :Document, :content [
            {:tag :Style, :attrs {:id "Poly1"}, :content [
                {:tag :LineStyle, :content [
                    {:tag :color, :content ["7f00ff00"]}
                    {:tag :width, :content ["2"]}
                ]}
                {:tag :PolyStyle, :content [
                    {:tag :color, :content ["7f00ff00"]}
                ]}
            ]}, ; End of style tag
            {:tag :Placemark, :content [
                {:tag :name, :content [(str filename)]},
                {:tag :description, :content [
                    {:tag :img, :attrs {:src (str filename ".jpg") :width "400"} }
                ]},
                {:tag :styleUrl, :content ["#Poly1"]},
                {:tag :Polygon, :content [
                    {:tag :altitudeMode, :content ["clampToGround"]},
                    {:tag :extrude, :content ["1"]},
                    {:tag :tessellate, :content ["1"]},
                    {:tag :outerBoundaryIs, :content [
                        {:tag :LinearRing, :content [
                            {:tag :coordinates, :content [
                                ; (to-coordinates-str coordinates)
                                coords-str
                            ]}
                        ]} ; End of LinearRing tag
                    ]} ; End of outerBoundaryIs tag
                ]} ; End of Polygon tag
            ]} ; End of Placemark tag
        ]} ; End of document tag
    ]})

(defn write-kml [filename coordinates]
  "Write kml file with name <filename> for coordinates <coordinates>"
  (spit (str filename ".computed.kml") (with-out-str (clojure.xml/emit (kml-tmpl filename (to-coordinates-str coordinates))))))
