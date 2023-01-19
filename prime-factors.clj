(ns prime-factors)

(defn of [n]
  (loop [n n, d 2, l (Math/sqrt n), arr []]
    (cond (= 1 n) arr
          (= 0 (rem n d)) (let [q (quot n d)
                                l (Math/sqrt q)]
                            (recur q d l (conj arr d)))
          (> d l) (recur n n n arr)
          :else (recur n (inc d) l arr))))

;; ------------------------
;; Using recursion --older implementation
(defn- nxt-factor [n d]
  (some #(when (= 0 (rem n %)) %) (iterate inc d)))

(defn of
  ([n] (of n 2 (Math/sqrt n)))
  ([n d l]
   (let [q (quot n d)
         r (rem n d)
         nl (Math/sqrt q)]
     (cond (= 1 n) []
           (= 0 r) (cons d (of q d nl))
           (> d l) (of n n n)
           :else (of n (nxt-factor n d) l)))))
