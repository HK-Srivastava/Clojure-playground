;; Provided below are three implementations to convert a number (in range [0, 999,999,999,999]) to words (in english)
;; Example: 34567 => "thirty-four thousand five hundred sixty-seven"
;;          1001001 => "one million one thousand one"
;; At the end, a comparison of the time taken by each implementation is provided.

(ns cljex.compare-time
  (:require [clojure.pprint :refer [cl-format]]
            [clojure.string :as str]))

;; ---------------------------------------------------------
;; Implementation-1 : Faster than built-in functionality, slower than Implementation-2

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

(defn num-to-word-1 [n]
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

;; ------------------------------------------------------------------------
;; Implementation-2 : Fastest of the three implementations

(def ^:private implicits
  {0 "zero"
   1 "one"
   2 "two"
   3 "three"
   4 "four"
   5 "five"
   6 "six"
   7 "seven"
   8 "eight"
   9 "nine"
   10 "ten"
   11 "eleven"
   12 "twelve"
   13 "thirteen"
   14 "fourteen"
   15 "fifteen"
   16 "sixteen"
   17 "seventeen"
   18 "eighteen"
   19 "nineteen"
   20 "twenty"
   30 "thirty"
   40 "forty"
   50 "fifty"
   60 "sixty"
   70 "seventy"
   80 "eighty"
   90 "ninety"})

(def ^:private ^:const mags
  [[1000000000 "billion"]
   [1000000    "million"]
   [1000       "thousand"]
   [100        "hundred"]])

(defn- -number [n]
  (cond (contains? implicits n) (implicits n)
        (< n 100) (let [r (rem n 10)]
                    (str (-number (- n r)) "-" (-number r)))
        :else
        (let [[[limit denom]] (drop-while (fn [[l]] (< n l)) mags)
              [qs r]          ((juxt (comp -number quot) rem) n limit)
              rs              (when-not (zero? r) (-number r))]
          (str/trimr (str/join " " [qs denom rs])))))

(defn num-to-word-2 [n]
  (if (not (<= 0 n 999999999999))
    (throw (IllegalArgumentException.))
    (-number n)))

;; -------------------------------------------------------
;; Implementation-3 : Using built-in functionality. Slowest of the three implementations.

(defn num-to-word-3 [n]
  (if (<= 0 n 999999999999)
    (str/replace
     (cl-format nil "~R" n)
     "," "")
    (throw (IllegalArgumentException.))))

;; ----------------------------------------------------
;; Comparison of time taken by each implementation

(time (dotimes [_ 1000]
        (num-to-word-1 6789054)))
;; "Elapsed time: 11.5878 msecs"
;; nil

(time (dotimes [_ 1000]
        (num-to-word-2 6789054)))
;; "Elapsed time: 3.4229 msecs"
;; nil

(time (dotimes [_ 1000]
        (num-to-word-3 6789054)))
;; "Elapsed time: 24.9286 msecs"
;; nil

