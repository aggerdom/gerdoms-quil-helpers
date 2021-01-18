(ns gerdoms-quil-helpers.interpolation)

;; TODO: Convert this to something more easily usable

;; Smoothing methods adapted from gfxil.net
;; https://sol.gfxile.net/interpolation/
(def smoothing-methods
  (let [smoothstep (fn [x] (* x x (- 3 (* 2 x))))
        catmullrom (fn [t p0 p1 p2 p3]
                     (* 0.5 (+ (* 2 p1)
                              (* (Math/pow t 1) (+ (* -1 p0) p2))
                              (* (Math/pow t 2) (+ (*  2 p0)
                                                   (* -5 p1)
                                                   (*  4 p2)
                                                   (* -1 p3)))
                              (* (Math/pow t 3) (+ (* -1 p0)
                                                   (*  3 p1)
                                                   (* -3 p2)
                                                   (*  1 p3))))))]
    {:linear identity
     :smoothstep smoothstep
     :smootherstep (fn [x] (* x x x (+ (* x (- (* x 6) 15)) 10)))
     :nth-power (fn [n x] (Math/pow x n))
     :nth-power-inv (fn [n x] (- 1 (Math/pow x n)))
     :nth-smooth-step (fn [n x] (nth (iterate smoothstep x) n))
     :sin (fn [x] (Math/sin (/ (* x Math/PI) 2)))
     :sin-inv (fn [x] (Math/asin (/ (* x Math/PI) 2))) ;; Todo: Check this
     :catmullrom-full catmullrom
     :catmullrom (fn [x Q T] (catmullrom x Q 0 1 T)) ;; Provide two control points Q and T
     }))

(defn interpolate-
  "Generate how far in range [1, 0] to interpolate in each of `n` steps with tween method `tween`.
   Output will be slightly confusing because it doesn't include the last point, and
   ranges from 1 to 0 in general.

  "
  ([a b n] (interpolate- a b n (smoothing-methods :linear)))
  ([a b n tween]
   (->> (range n)
        (map #(/ % n))
        (map (cond
               (fn? tween) tween
               (keyword? tween) (smoothing-methods tween)
               (string? tween) (smoothing-methods (keyword tween))))
        (map #(+ (* a %)
                 (* b (- 1 %)))))))

(defn interpolate
  "Get interpolation from `a` to `b` in `n` steps using a given `tween` method.

   :EXAMPLE:
      (interpolate 0 10 5 :linear) ;; => [0 2 4 6 8 10]"
  ([a b n] (interpolate a b n (smoothing-methods :linear)))
  ([a b n tween]
   (let [difference (- b a)
         percentages (interpolate- 0 1 n tween) ;; Last will be n-1
         offsetpcts  (map #(- 1 %) percentages)
         pts         (map #(+ a (* difference %)) offsetpcts)
         inclusive   (conj (vec pts) b)]
     inclusive)))

(interpolate 0 10 5) ;; => [0 2 4 6 8 10]
(interpolate 10 0 5) ;; => [10 8 6 4 2 0]

