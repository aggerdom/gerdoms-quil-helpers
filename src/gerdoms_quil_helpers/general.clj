(ns gerdoms-quil-helpers.general)

(defn multiple? [a b]
  "Returns true if `a` is a multiple of `b` else false"
  (= 0 (mod a b)))

(defn gcd [a b]
  "Greatest common divisor of two numbers `a` and `b`"
  (if (zero? b) a
      (recur b (mod a b))))

(defn lcm
  "Least common multiple of two numbers `a` and `b`"
  ([a] a)
  ([a b] (/ (* a b) (gcd a b)))
  ([a b & rest] (reduce lcm (concat [a b] rest))))

;; (deftest lcm-test
;;   (testing "Single Arity Should Work"
;;     (is (== (lcm 4) 4)))
;;   (testing "Two Arity Should Work"
;;     (is (== (lcm 2 4) 4)))
;;   (testing "Three Arity Should Work")
;;   (testing "All Arities Equivalent"
;;     (is (== (lcm 2) (lcm 2 2) (lcm 2 2 2))))
;;   (testing "Positive"
;;     (is (== (lcm 7 5/7 2 3/5) 210))
;;     (is (== (lcm 7 5/7 2 3/5) 210))
;;     (is (== (lcm 5 3 7) 105))))

(defn mean [& xs]
  (/ (reduce + xs)
     (count xs)))


(defn clamp [x lower upper]
  {:pre [(<= lower upper)]}
  (cond (< x lower) lower
        (> x upper) upper
        :else x))

(defn sum-of-squares [xs]
  "Sum of squares for sequence of xs"
  (->> xs
       (map #(* % %))
       (reduce +)))

(defn square-of-sums [xs]
  "Square of sums for sequence of xs"
  (->> xs
       (reduce +)
       (#(* % %))))

(defn floor-mag [x]
  "Floor a number to the lowest number of the same magnitude.
   Undefined at 0 and will throw IllegalArgumentException.

   :Examples:
   (floor-mag 1) ;; => 1
   (floor-mag 99) ;; => 10
   (floor-mag 101 ;; => 100)"
  (int (Math/pow 10 (int (Math/log10 x)))))

