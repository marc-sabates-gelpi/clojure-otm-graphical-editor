(ns otm-graphical-editor.core
  (:require [clojure.string :as s]
            [clojure.set :refer [union difference]]
            [otm-graphical-editor.image :as i])
  (:gen-class))

(defn- string->char-or-num
  "Returns char or number"
  [x]
  (let [xx (read-string x)]
    (if (number? xx) xx (first (seq x)))))

(defmulti run-cmd
  "Execute image commands"
  (fn [_ cmd _] cmd))

(defmethod run-cmd :i
  ^{:doc "I M N​
   Create a new M x N image with all pixels coloured white (O)"}
  [_ _ [cols rows]]
  (i/make-image cols rows i/colour-white))

(defmethod run-cmd :c
  ^{:doc "C​
   Clears the table, setting all pixels to white (O)"}
  [image & _]
  (i/make-image (i/get-num-cols image) (i/get-num-rows image) i/colour-white))

(defmethod run-cmd :l
  ^{:doc "L X Y C​
   Colours the pixel (X,Y) with colour C"}
  [image _ [col row colour]]
  (let [pixel [col row]]
    (if (i/get-pixel image pixel)
      (i/set-pixel image pixel (or colour i/colour-white))
      image)))

(defmethod run-cmd :v
  ^{:doc "V X Y1 Y2 C​
   Draw a vertical segment of colour C in column X between rows Y1 and 
   Y2 (inclusive)"}
  [image _ [col r1 r2 colour]]
  (let [pixels (for [r (range r1 (inc r2))] [col r])
        colour (or colour i/colour-white)]
    (if (and
         (i/get-pixel image [col r1])
         (i/get-pixel image [col r2]))
      (reduce (fn [image pixel] (i/set-pixel image pixel colour)) image pixels)
      image)))

(defmethod run-cmd :h
  ^{:doc "H X1 X2 Y C​
   Draw a horizontal segment of colour C in row Y between columns X1 and
   X2 (inclusive)"}
  [image _ [c1 c2 row colour]]
  (let [pixels (for [c (range c1 (inc c2))] [c row])
        colour (or colour i/colour-white)]
    (if (and
         (i/get-pixel image [c1 row])
         (i/get-pixel image [c2 row]))
      (reduce (fn [image pixel] (i/set-pixel image pixel colour)) image pixels)
      image)))

(defn- adjacents
  "Gets a set of valid adjacent pixel coordinates"
  [image [c r]]
  (->> (into
        (for [cc '(1 -1)] [(+ c cc) r])
        (for [rr '(1 -1)] [c (+ r rr)]))
       (filter #(let [v (i/get-pixel image %)] (if v %)))
       set))
 
(defn- find-region
  "Gets the set of pixel's coordinates for the pixel's region"
  [image pixel]
  (let [colour (i/get-pixel image pixel)]
    (loop [region #{} visited #{} to-visit #{pixel}]
      (if (empty? to-visit)
        region
        (let [current (first to-visit)]
          (recur
           (if (= colour (i/get-pixel image current))
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
  (let [pixel [col row]]
    (reduce (fn [image pixel] (i/set-pixel image pixel colour)) image (find-region image pixel))))

(defmethod run-cmd :s
  ^{:doc "S​
   Show the contents of the current image"}
  [image & _]
  (do
    (doseq [x (i/get-rows image)] (println (apply str x)))
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
