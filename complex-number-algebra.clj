(ns complex-numbers)

(defn real [[a b]] a)

(defn imaginary [[a b]] b)

(defn abs [[a b]] 
  (Math/sqrt (+ (* a a) (* b b))))

(defn conjugate [[a b]]
  [a (- b)])

(defn add [[a b] [c d]] 
  [(+ a c) (+ b d)])

(defn sub [[a b] [c d]] 
  [(- a c) (- b d)])

(defn mul [[a b] [c d]] 
  [(- (* a c) (* b d)) (+ (* a d) (* b c))])

(defn div [[a b] [c d]]
  {:pre [(not (= 0 c d))]}
  (let [denom (double (+ (* c c) (* d d)))
        num1 (double (+ (* a c) (* b d)))
        num2 (double (- (* b c) (* a d)))]
    [(/ num1 denom) (/ num2 denom)]))


;; ----------------------------------------------------------

;; Implementation of above code using macros

(defn- proc [f1 f2 [a b] [c d]]
  [(f1 (* a c) (* b d))
   (f2 (* b c) (* a d))])

(defn- sqr [[a b]]
  (+ (* a a) (* b b)))

(def real first)

(def imaginary second)

(def abs #(Math/sqrt (sqr %)))

(def conjugate #(update % 1 -))

(def add (partial map +))

(def sub (partial map -))

(def mul (partial proc - +))

(defn div [n1 [c d :as n2]]
  (when-not (== 0 c d)
    (let [division #(/ % (double (sqr n2)))]
      (proc (comp division +) (comp division -)
            n1 n2))))

