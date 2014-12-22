(require '[clojure.xml :as xml] '[clojure.string :as string])

(defn to-coordinates-str [coords]
  "convert incoming list of vectors to strings"
  ;mario ersetzt #(string/join "," %) mit  #(str (string/join "0," %) "0") zum Testen gegen
  ; die truth.kml Da stehen halt 0er am Ende, kann final wieder geändert werden
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
  (spit (str filename ".kml") (with-out-str (clojure.xml/emit (kml-tmpl filename (to-coordinates-str coordinates))))))


(def coordinates-in
  '(["5.3506056" "50.9344353"] ["5.3508901" "50.9345318"] ["5.3511934" "50.9345868"] ["5.3510527" "50.9346006"] ["5.3512893" "50.9346879"] ["5.3513196" "50.9347198"] ["5.3497853" "50.9347811"] ["5.3512742" "50.9348326"] ["5.3498475" "50.9348581"] ["5.3500446" "50.9350967"] ["5.3502773" "50.9353510"] ["5.3510358" "50.9354012"] ["5.3506481" "50.9354423"] ["5.3503873" "50.9355158"] ["5.3502677" "50.9353366"] ["5.3497302" "50.9347074"] ["5.3507889" "50.9343783"] ["5.3513908" "50.9349383"] ["5.3511612" "50.9354182"] ["5.3507947" "50.9355094"] ["5.3497996" "50.9348001"])
)
(map #(str %1 "0") coordinates-in)
(write-kml "0011" coordinates-in)
