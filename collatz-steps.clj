(ns collatz-steps)
;; The Collatz Conjecture or 3x+1 problem can be summarized as follows:
;;    Take any positive integer n. 
;;    If n is even, divide n by 2 to get n / 2. 
;;    If n is odd, multiply n by 3 and add 1 to get 3n + 1. 
;;    Repeat the process indefinitely. 
;; The conjecture states that no matter which number you start with, you will always reach 1 eventually.
;; Given a positive integer n, the function below returns the number of steps required to reach 1.

(defn collatz [n]
  (let [step #(if (even? %) (quot % 2) (+ 1 (* 3 %)))]
    (if (= n 1)
      0
      (inc (collatz (step n))))))
