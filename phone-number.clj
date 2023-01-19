(ns phone-number
  (:require [clojure.string :as str]))

(def ^:private numzero "0000000000")

(defn- validate [num-string]
  (let [uno (first num-string)
        tres (nth num-string 3)]
    (cond (= \0 uno) numzero
          (= \1 uno) numzero
          (= \0 tres) numzero
          (= \1 tres) numzero
          :else num-string)))

(defn number [num-string]
  (let [num (str/join "" (str/split num-string #"\D"))
        len (count num)]
    (cond 
      (and (= len 11) (= \1 (first num))) (validate (subs num 1))
      (= len 10) (validate num)
      :else numzero)))

(defn area-code [num-string]
  (let [ac  (subs (number num-string) 0 3)
        uno (first ac)]
    (when-not (or (= \0 uno) (=\1 uno)) ac)))

(defn pretty-print [num-string]
  (let [num (number num-string)
        regx #"(\d{3})(\d{3})(\d{4})"
        [_ ac mid end] (re-matches regx num)]
    (format "(%s) %s-%s" ac mid end)))
