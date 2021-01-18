(ns gerdoms-quil-helpers.combinatorics)

;; K-combinations (P103)
(defn k-combination [k S]
  (if (= 0 k) [#{}]
      (set
       (for [el   S
             rest (k-combination (dec k) (clojure.set/difference S #{el}))]
         (conj rest el)))))

;; (deftest -k-combination-test
;;   (testing "Examples from site"
;;     (is (= (k-combination 2 #{4 5 6})
;;            #{#{6 5} #{4 6} #{4 5}}))))
