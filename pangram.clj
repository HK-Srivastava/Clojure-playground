(ns pangram
  (:require [clojure.string :as str]
            [clojure.set :as set]))

(defn pangram? [s] 
  (let [s (set (str/lower-case s))
        letters (set "abcdefghijklmnopqrstuvwxyz")]
    (set/subset? letters s)))
