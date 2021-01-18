(ns gerdoms-quil-helpers.geometry)

(defn circle-coord
  ([h k r t] (let [x (+ (* r (Math/cos t)) h)
                   y (+ (* r (Math/sin t)) k)]
               [x y]))
  ([[h k] r t] (circle-coord h k r t)))

(defn slope [p q]
  (let [[x1 y1] p
        [x2 y2] q]
    (/ (- y2 y1) (- x2 x1))))

(defn centroid
  "Calculates a centroid, or center point, from a sequence of points"
  [points]
  (->> (apply map vector points)
       (map #(reduce + %))
       (map #(/ % (count points)))))

;; (deftest test-centroid
;;   (testing "Basic test"
;;     (is (= (centroid [[0 0] [2 2] [0 2] [2 1]])
;;            [1 5/4]))
;;     (is (= (centroid [[0 0] [0 1] [1 1] [1 0]])
;;            [1/2 1/2]))))


;; TODO: Deal with vertical points
(defn colinear [a b c] (= (slope a b) (slope b c) (slope a c)))

(colinear [0 0] [1 0] [2 0])
(colinear [0 0] [1 1] [2 0])

(defn sort-clockwise
  "Sort a list of points clockwise around their center"
  [points]
  (let [[cx cy] (centroid points)
        get-angle (fn [[x y]]
                    (mod (+ 360
                            (Math/toDegrees
                             (Math/atan2 (- x cx) (- y cy)))) 360))]
    (->> points
         (sort-by get-angle))))


(comment (centroid [[0 0] [2 2] [0 2] [2 1]]))


(defn minkowski-distance
  "Construct distance function for Minkowski metric of a given order."
  [order]
  (let [power (/ 1.0 order)
        fdist (fn [p q]
                (->> (map vector p q)
                     (map #(reduce - %))
                     (map #(Math/abs %))
                     (reduce +)
                     (#(Math/pow % power))))]
    fdist))


(def manhattan-distance (minkowski-distance 1.0))
(def euclidian-distance (minkowski-distance 2.0))
(def chebyshev-distance (minkowski-distance 13.0))

(comment
  (manhattan-distance [1 1] [0 0])
  (euclidian-distance [1 1] [0 0])
  (chebyshev-distance [1 1] [0 0]))


