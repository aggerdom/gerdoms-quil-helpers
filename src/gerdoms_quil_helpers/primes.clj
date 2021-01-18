(ns gerdoms-quil-helpers.primes
  (:require [gerdoms-quil-helpers.general :as g]))
;;;; Streams

(defn primes
  "Prime Number Stream.
   Currently uses naive implementation."
  ([] (lazy-seq (cons 2 (primes 3 #{2}))))
  ([x found]
   (if (some #(zero? (mod x %)) found)
     (primes (+ 2 x) found)
     (lazy-seq (cons x (primes (+ 2 x) (conj found x)))))))

(defn prime? [x]
  "Returns true if x is prime, otherwise false."
  (= x (first (drop-while #(< % x) (primes)))))

(defn prime-factor [x]
  {:pre [(< 0 x)]}
  "Find a prime factor of a number x"
  (cond (= x 1) 1     ;; Prime factor of 1 is 1 by definition
        :else (->> (primes)
                   (drop-while #(not (g/multiple? x %)))
                   (first))))

(defn prime-factors [x]
  #{:pre [(< 0 x)]}
  "Prime factorization of x"
  (let [p (prime-factor x)
        q (quot x p)]
    (if (= q 1) [p]
        (lazy-seq (cons p (prime-factors q))))))

(defn balanced-primes
  "Lazy seq of balanced primes.
  A balanced prime is a prime that is the mean of the preceeding
  and following prime numbers."
  []
  (->> (primes)
       (partition 3 1)
       (filter #(= (second %) (g/mean (first %) (last %))))
       (map second)))


(defn coprimes
  "Sequence containing all pairs of coprime numbers."
  ([]
   (interleave
    (coprimes 2 1)    ;; Odd+Even/Odd+Even Pairs
    (coprimes 3 1)))  ;; Odd+Odd Pairs
  ([m n]
   (lazy-seq
    (cons [m n]
          (interleave
           (coprimes (- (* 2 m) n) m)
           (coprimes (+ (* 2 m) n) m)
           (coprimes (+ m (* 2 n)) n))))))


(take 10 (coprimes))

