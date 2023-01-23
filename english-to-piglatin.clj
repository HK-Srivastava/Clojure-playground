;; Implement a program that translates from English to Pig Latin.
;; Pig Latin is a made-up children's language that's intended to be confusing.
;; It obeys a few simple rules (below), but when it's spoken quickly it's really difficult for non-children (and non-native speakers) to understand.
;;    Rule 1: If a word begins with a vowel sound, add an "ay" sound to the end of the word. Please note that "xr" and "yt" at the beginning of a word make vowel sounds (e.g. "xray" -> "xrayay", "yttria" -> "yttriaay").
;;    Rule 2: If a word begins with a consonant sound, move it to the end of the word and then add an "ay" sound to the end of the word. Consonant sounds can be made up of multiple consonants, a.k.a. a consonant cluster (e.g. "chair" -> "airchay").
;;    Rule 3: If a word starts with a consonant sound followed by "qu", move it to the end of the word, and then add an "ay" sound to the end of the word (e.g. "square" -> "aresquay").
;;    Rule 4: If a word contains a "y" after a consonant cluster or as the second letter in a two letter word it makes a vowel sound (e.g. "rhythm" -> "ythmrhay", "my" -> "ymay").
;; There are a few more rules for edge cases, and there are regional variants too.
;; See http://en.wikipedia.org/wiki/Pig_latin for more details.

(ns pig-latin
  (:require [clojure.string :as str]))

(defn- translate-word [word]
    (cond
      (and (= 2 (count word)) 
           (= \y (last word))) (str "y" (first word) "ay")
      (re-find #"^[aeiou]|^xr|^yt" word) (str word "ay")
      :else
        (let [[_ remaining ist] (re-matches #"(.*[aeiou])(.*)" (str/reverse word)) ;; Here `ist` is consonant cluster and `remaining` is rest of the word
              [ist remaining] (if (nil? ist)
                                [word ""] 
                                (map str/reverse [ist remaining]))
              [ist-y & rest-y] (str/split ist #"y")
              rest-y (if (nil? rest-y) "" (str/join #"qu" rest-y))]
          (cond
            (and (not= ist-y ist)
                 (not= \y (first ist))) (str "y" rest-y remaining ist-y "ay")
            (and (= \q (last ist))
                 (= \u (first remaining))) (str (subs remaining 1) ist "uay")
            :else (str remaining ist "ay")))))

(defn translate [phrase]
  (->> (str/split phrase #" ")
      (map translate-word)
      (str/join #" ")))
