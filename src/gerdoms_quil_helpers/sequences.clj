(ns gerdoms-quil-helpers.sequences)

(defn roll
  "Rotate a sequence `n` steps"
  [n xs]
  (let [roll-forward #(concat (drop 1 %) [(first %)])
        roll-back    #(concat [(last %)] (butlast %))]
    (cond
      (> n 0) (nth (iterate roll-forward xs) n)
      (< n 0) (nth (iterate roll-back xs) (- n))
      :else xs)))

(defn min-common
  "Find minimum common element in provided collections"
  [& colls]
  (let [minimums (map first colls)
        to-find  (apply max minimums)
        dropped  (map (partial drop-while #(> to-find %)) colls)
        all-comm (= #{to-find} (set minimums))]
    (if all-comm to-find
        (apply min-common dropped))))

(comment
  (min-common (range) (range 0 100 11) [2 3 5 7 11 13]) ;; => 11
  (min-common (range) (range 0 100 7/6) [2 3 5 7 11 13])) ;; => 7

(defn zipseqs
  "Similar to Python's zip, lazily produce from each sequence
  in a list of sequences"
  ([& seqs]
   (if (some empty? seqs) '()
       (lazy-seq (cons (map first seqs)
                       (apply zipseqs (map rest seqs)))))))

(defn vsegment
  "Given a list of numbers of elements to take in sequence,
   lazily yield that number of elements from xs until exhausted.

  :EXAMPLE:

  (segment [2 3] [1 2 3 4 5 6 7 8 9 10 11])
  ;; => ((1 2) (3 4 5) (6 7) (8 9 10)) "
  [block-sizes xs]
  (if (not= (first block-sizes)
            (count (take (first block-sizes) xs))) '()
      (lazy-seq
       (cons (take (first block-sizes) xs)
             (vsegment (concat (rest block-sizes) [(first block-sizes)])
                      (drop (first block-sizes) xs))))))

(defn vsegment-all
  "Given a list of numbers of elements to take in sequence,
   lazily yield that number of elements from xs until exhausted.
   Final element may not contain the requested number of elements.

  :EXAMPLE:
    (segment [2 3] [1 2 3 4 5 6 7 8 9 10 11])
    ;; => ((1 2) (3 4 5) (6 7) (8 9 10)) "

  [block-sizes xs]
  (if (empty? xs) '()
      (lazy-seq
       (cons (take (first block-sizes) xs)
             (vsegment-all (concat (rest block-sizes) [(first block-sizes)])
                          (drop (first block-sizes) xs))))))

(defn fib
  "(Lazy) Fibonacci Sequence
   When called without arguments, assumes seed `a`=1, `b`=1).
   When called with arguments, assumes `a` and `b` to be provided values."
  ([] (fib 0 1))
  ([a b] (lazy-cat [a]
                   (fib b (+' a b)))))

