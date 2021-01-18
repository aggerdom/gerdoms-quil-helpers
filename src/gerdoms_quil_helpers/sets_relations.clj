(ns gerdoms-quil-helpers.sets-relations)

(defn eq-classes
  "Equivalence classes for a domain `D` wrt `f`"
  [f D]
  (->> (group-by f D) (vals) (map set) (set)))

(eq-classes #(* % %) #{-2 -1 0 1 2})

(defn transitive-closure [R]
  "Transitive closure for a binary relation `R`.
   `R` should be represented as set of domain+range tuples"
  (letfn
   [(succ [R]
      (into R (for [[a b] R
                    [c d] R
                    :when (= b c)]
                [a d])))]
    (->> R
         (iterate succ)
         (partition 2 1)
         (drop-while (fn [[Rn-1 Rn]] (not= Rn-1 Rn)))
         (ffirst))))

(transitive-closure #{[8 4] [9 3] [4 2] [27 9]}) ;; => #{[27 9] [27 3] [8 4] [4 2] [9 3] [8 2]}

;; Problem 85: Powerset
;; http://www.4clojure.com/problem/85
(defn powerset
  [s]
  (if (empty? s) #{#{}}
      (let [e (first s)
            more (disj s e)
            subsets-without (into #{} (powerset more))
            subsets-with (map #(conj % e) subsets-without)]
        (->> (clojure.set/union subsets-with subsets-without)
             (set)))))

(powerset #{1 :a}) ;; => #{#{1 :a} #{} #{1} #{:a}}

(defn pairwise-disjoint? [sets]
  "True if no sets in list of sets overlaps"
  (let [sets (vec sets)]
    (every? true?
            (for [i (range (count sets))
                  j (range (count sets))
                  :when (not= i j)]
              (= #{} (clojure.set/intersection
                      (nth sets i) (nth sets j)))))))

(pairwise-disjoint? #{#{\U} #{\s} #{\e \R \E} #{\P \L} #{\.}})
(pairwise-disjoint? #{#{\U} #{\s \U} #{\e \R \E} #{\P \L} #{\.}}) ;; => false

