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
  ([a b n] (interpolate a b n (smoothing-methods :linear)))
  ([a b n tween]
   (->> (range n)
        (map #(/ % n))
        (map tween)
        (map #(+ (* a %)
                 (* b (- 1 %)))))))

((defn interpolate
   ([a b n] (interpolate a b n (smoothing-methods :linear)))
   ([a b n tween]
    (->> (interpolate- a b n tween))))
 1
 2
 3)

(100 - 80N)
