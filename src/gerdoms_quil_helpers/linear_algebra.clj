(ns gerdoms-quil-helpers.linear-algebra
  (:require [clojure.core.matrix :as m]))


(defn nrows [M] (count M))
(defn ncols [M] (count (first M)))
(defn size [M] [(nrows M) (ncols M)])
(defn transpose [M] (apply mapv vector M)) 
;; Test: (is (transpose (traspose A)) A)
;; Test: (is (transpose (mmul A B)) (mmul (transpose A) (transpose B)))
(defn mmul-able? [A B] (= (ncols A) (nrows B)))
(defn mmulled-size [A B] [(nrows A) (ncols B)])

;; Rotations
(defn R2 [theta]
  [[(m/cos theta) (- (m/sin theta))]
   [(m/sin theta)    (m/cos theta)]])

(defn R3x [theta]
  [[1 0                 0]
   [0 (m/cos theta) (- (m/sin theta))]
   [0 (m/sin theta)    (m/cos theta)]])

(defn R3y [theta]
  [[(m/cos theta)     0 (m/sin theta)]
   [0                 1 0            ]
   [(- (m/sin theta)) 0 (m/cos theta)]])

(defn R3z [theta]
  [[(m/cos theta) (- (m/sin theta)) 0]
   [(m/sin theta) (m/cos theta)     0]
   [0             0                 1]])

;; Problem 143: Dot product
(defn dot-product [p q]
  (->> (interleave p q)
       (partition 2)
       (map (partial apply *))
       (reduce +)))

(dot-product [0 1 0] [1 0 0]) ;; => 0
(dot-product [1 1 0] [1 0 0]) ;; => 1
(dot-product [1 1 0] [1 -1 0]) ;; => 0

