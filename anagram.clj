(ns anagram
  (:require [clojure.string :refer [lower-case]]))
;; Returns all the words from a given sequence which are anagrams of a given word.
;; Ignores the case of the letters when checking for anagram but doesn't change the case of letters,
;;    i.e the anagram words in output are in the same case as they were in the input sequence
;; Doesn't return the same word as anagram i.e the output sequence will not have the input word (ignoring case) whose anagrams are being returned

(defn anagrams-for [word prospect-list] ;;
  (let [word (lower-case word)
        prospect-list (filter #(not (= word (lower-case %))) prospect-list)
        word (sort word)
        fn-slc (comp sort lower-case)]
    (filter #(= word (fn-slc %)) prospect-list)))
