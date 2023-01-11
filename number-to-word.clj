(ns cljex.num-to-word 
  (:require [clojure.pprint :refer [cl-format]]
            [clojure.string :as cs]))

;; --------------------------------------------------
;; Without using built-in functionality

(def ones {\0 "" \1 "one", \2 "two", \3 "three", \4 "four", \5 "five", \6 "six",
           \7 "seven", \8 "eight", \9 "nine"})

(def tens {\0 "",
           \1 {\0 "ten" \1 "eleven", \2 "twelve", \3 "thirteen", \4 "fourteen",
               \5 "fifteen", \6 "sixteen", \7 "seventeen", \8 "eighteen", \9 "nineteen"},
           \2 "twenty", \3 "thirty", \4 "forty", \5 "fifty",
           \6 "sixty", \7 "seventy", \8 "eighty", \9 "ninety"})

(def hundreds {\0 "" \1 "one hundred", \2 "two hundred", \3 "three hundred", \4 "four hundred",
               \5 "five hundred", \6 "six hundred", \7 "seven hundred", \8 "eight hundred", \9 "nine hundred"})

(defn- ones-tens [o t]
  (if (= \1 t)
    [((tens \1) o)]
    (->> [(tens t) (ones o)]
         (remove #(or (nil? %) (empty? %)))
         (interpose "-")
         (apply str)
         vector)))

(defn- parse-item [item]
  (let [label (keys item)
        [o t h] (first (vals item))]
    (if (= \0 o t h)
      '()
      (concat label (ones-tens o t) [(hundreds h)]))))

(defn number [n]
  (cond
    (= 0 n) "zero"
    (<= 1 n 999999999999)
    (let [scale ["" "thousand" "million" "billion"]
          nil-empty? #(or (nil? %) (empty? %))]
      (->> n str reverse
           (partition-all 3)
           (map hash-map scale)
           (reduce #(into %1 (parse-item %2)) '())
           (remove nil-empty?)
           (interpose " ")
           (apply str)))
    :else (throw (IllegalArgumentException. "Number out of range"))))

;; -------------------------------------------------
;; Using built-in functionality

(defn number [n]
  (if (<= 0 n 999999999999)
    (cs/replace
     (cl-format nil "~R" n)
     "," "")
    (throw (IllegalArgumentException.))))

