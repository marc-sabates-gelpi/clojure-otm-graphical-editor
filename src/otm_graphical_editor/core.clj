(ns otm-graphical-editor.core
  (:require [clojure.string :as s]
            [clojure.set :refer [union difference]])
  (:gen-class))

(def ^:const colour-white \O)

(defn- string->char-or-num
  "Returns char or number"
  [x]
  (let [xx (read-string x)]
    (if (number? xx) xx (first (seq x)))))

(defn- make-image
  "Makes a cols x rows image with the especified colour"
  [cols rows & [colour & _]]
  (let [colour (or colour colour-white)
        r (vec (repeat rows colour))]
    (vec (repeat cols r))))

(defmulti run-cmd
  "Execute image commands"
  (fn [_ cmd _] cmd))

(defmethod run-cmd :i
  ^{:doc "I M N​
   Create a new M x N image with all pixels coloured white (O)"}
  [_ _ [cols rows]]
  (make-image rows cols))

(defmethod run-cmd :c
  ^{:doc "C​
   Clears the table, setting all pixels to white (O)"}
  [image & _]
  (make-image (count (first image)) (count image)))

(defmethod run-cmd :l
  ^{:doc "L X Y C​
   Colours the pixel (X,Y) with colour C"}
  [image _ [col row colour]]
  (let [pixel [(dec row) (dec col)]]
    (if (get-in image pixel)
      (assoc-in image pixel (or colour colour-white))
      image)))

(defmethod run-cmd :v
  ^{:doc "V X Y1 Y2 C​
   Draw a vertical segment of colour C in column X between rows Y1 and 
   Y2 (inclusive)"}
  [image _ [col r1 r2 colour]]
  (let [col (dec col)
        pixels (for [r (range (dec r1) r2)] [r col])
        colour (or colour colour-white)]
    (if (and
         (get-in image [(dec r1) col])
         (get-in image [(dec r2) col]))
      (reduce (fn [image pixel] (assoc-in image pixel colour)) image pixels)
      image)))

(defmethod run-cmd :h
  ^{:doc "H X1 X2 Y C​
   Draw a horizontal segment of colour C in row Y between columns X1 and
   X2 (inclusive)"}
  [image _ [c1 c2 row colour]]
  (let [row (dec row)
        pixels (for [c (range (dec c1) c2)] [row c])
        colour (or colour colour-white)]
    (if (and
         (get-in image [row (dec c1)])
         (get-in image [row (dec c2)]))
      (reduce (fn [image pixel] (assoc-in image pixel colour)) image pixels)
      image)))

(defn- adjacents
  "Gets a set of valid adjacent pixel coordinates"
  [image [x y]]
  (->> (into
        (for [xx '(1 -1)] [(+ x xx) y])
        (for [yy '(1 -1)] [x (+ y yy)]))
       (filter #(let [v (get-in image %)] (if v %)))
       set))
 
(defn- find-region
  "Gets the set of pixel's coordinates for the pixel's region"
  [image pixel]
  (let [colour (get-in image pixel)]
    (loop [region #{} visited #{} to-visit #{pixel}]
      (if (empty? to-visit)
        region
        (let [current (first to-visit)]
          (recur
           (if (= colour (get-in image current))
             (conj region current)
             region)
           (conj visited current)
           (union
            (disj to-visit current)
            (difference (adjacents image current) visited))))))))

(defmethod run-cmd :f
  ^{:doc "F X Y C​
   Fill the region R with the colour C. R is defined as: Pixel (X,Y)
   belongs to R. Any other pixel which is the same colour as (X,Y) and
   shares a common side with any pixel in R also belongs to this region"}
  [image _ [col row colour]]
  (let [row (dec row) col (dec col)]
    (reduce (fn [image pixel] (assoc-in image pixel colour)) image (find-region image [row col]))))

(defmethod run-cmd :s
  ^{:doc "S​
   Show the contents of the current image"}
  [image & _]
  (do
    (doseq [x image] (println (apply str x)))
    image))

(defmethod run-cmd :x
  ^{:doc "X​
   Terminate the session"}
  [image & _]
  image)

(defn- run-interactive
  "Runs the editor from the standard input"
  []
  (loop [image [] line (read-line)]
    (let [[cmd & args] (s/split line #" ")
          cmd (keyword (s/lower-case cmd))
          args (map string->char-or-num args)]
      (if (not= :x cmd)
        (recur (run-cmd image cmd args) (read-line))))))

(defn- run-from-file [file]
  "Runs the editor in batch mode"
  (-> file
      slurp
      (s/split-lines)
      (as-> lines (transduce (comp
                              (map #(s/split % #" "))
                              (map (fn [[cmd & args]]
                                     [(keyword (s/lower-case cmd))
                                      (map string->char-or-num args)]))
                              (halt-when #(= % :x)))
                             (completing (fn [image [cmd args]]
                                           (run-cmd image cmd args)))
                             []
                             lines))))

(defn -main
  "Runs the graphical editor"
  [& args]
  (if (seq args)
    (run-from-file (first args))
    (run-interactive)))
