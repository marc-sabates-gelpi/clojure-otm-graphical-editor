(ns otm-graphical-editor.image
  (:gen-class))

;;;; Image implementation with a vector of vectors.

;;; Each vector in the main vector is a row.
;;; Each element in the subvector is a pixel.
;;; Each pixel is a char representing a colour.
;;; Each pixel position is identified by.
;;; [col row] int values starting from 1.
;;; All coordinates are swapped and decremented.
;;;
;;; ex: [[\A \O \O]
;;;      [\B \O \O]
;;;      [\O \O \C]]
;;; pixel   internal pixel   colour
;;; [1 1]   [0 0]            \A
;;; [1 2]   [1 0]            \B
;;; [3 3]   [2 2]            \C

(def ^:const colour-white \O)

(defn- pixel->internal-pixel
  "Converts pixel coordinates."
  [[col row]]
  (vector (dec row) (dec col)))

(defn make-empty-image
  "Makes and empty image."
  []
  [])

(defn make-image
  "Makes a cols x rows image with the especified colour."
  ([cols rows]
   (make-image cols rows colour-white))
  ([cols rows colour]
   (let [r (vec (repeat cols colour))]
     (vec (repeat rows r)))))

(defn set-pixel
  "Sets the colour of a pixel."
  [image pixel colour]
  (assoc-in image (pixel->internal-pixel pixel) colour))

(defn get-pixel
  "Gets the colour of pixel.
   Returns nil if the pixel doesn't exist."
  [image pixel]
  (get-in image (pixel->internal-pixel pixel)))

(defn get-num-cols
  "Gets num cols."
  [[a-row & _]]
  (count a-row))

(defn get-num-rows
  "Gets num rows."
  [image]
  (count image))

(defn get-rows
  "Gets a collection of all the rows."
  [image]
  (seq image))
