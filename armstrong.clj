(ns armstrong-numbers)
; An armstrong number is a number which is equal to the sum of its digits raised to the power of the number of digits
; This code works for big integers (tested for a 17 digit armstrong number: 21897142587612075)

(defn get-digits [n] ; function that takes a number and returns a list of its digits
  (if (< n 10)
    [n]
    (conj (get-digits (quot n 10)) (rem n 10))))

; custom function to calculate exponent because Math/pow gave inaccurate results for big integers
(comment (defn expt [base exponent] 
  (if (zero? exponent)
    1
    (* base (expt base (dec exponent))))))

; Better, more readable implementation of the expt function. The above implementation also works fine.
(defn expt [base exponent]
  (apply * (repeat exponent base)))

(comment (defn armstrong? [n] ;; <- arglist goes here
  ;; your code goes here
  (= n (apply + (map
                    #(expt % (count (get-digits n)))
                    (get-digits n))))))

; the above implementation works fine but I decided that the below one is more readable so I commented the above implementation
(defn armstrong? [n] 
  (let [digits (get-digits n)
        digit-count (count digits)
        pow-digits (map #(expt % digit-count) digits)
        sumnum (apply + pow-digits)]
    (= n sumnum)))
