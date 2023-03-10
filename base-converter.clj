(ns base-converter)
;; This script is to convert any sequence of digits from one base to a sequence of digits in another base.
;; Only supports positive numbers.
;; Returns nill for unusual cases and empty list if the provided sequence of digits is empty

(comment  ;; New implementation of to-decimal makes the following code obsolete/unnecessary.

(defn- expt [base exponent] ;; Used to calculate exponent, behaves like the standard Math/pow function
  (apply * (repeat exponent base)))

;; Generates a list of n place values in a given base starting from ones place. 
;; eg: In decimal, for n=4 => [1000, 100, 10, 1]. In binary, for n=4 => [8, 4, 2, 1]
(defn- powers-of-base [base n]
  (map #(expt base %) (range (dec n) -1 -1)))

(defn- to-decimal [base digits]  ;; converts a number (taken as a list of its digits) to decimal
  (let [digit-count (count digits)
        place-values (powers-of-base base digit-count)]
    (apply + (map * digits place-values))))
    
(defn- to-decimal [base digits] ;; More readable implementation of the above to-decimal function that has been commented out.
  (->> (count digits)
       (powers-of-base base)
       (map * digits)
       (apply +)))
)
  
(defn- to-decimal [base digits]
  (reduce #(+ (* base %1) %2) 0 digits))

(defn- to-base [base n] ;; converts a decimal number to a sequence of its digits in any given base
  (if (< n base)
    [n]
    (conj (to-base base (quot n base)) (rem n base))))

(defn convert [base-a digits base-b] ;; the converter function
  (cond 
    (or (some neg? digits)
        (some #(>= % base-a) digits)
        (<= base-a 1)
        (<= base-b 1)) nil
    (empty? digits) '()
    :else (to-base base-b (to-decimal base-a digits))))
