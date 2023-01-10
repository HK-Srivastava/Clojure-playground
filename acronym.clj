(ns acronym
  (:require [clojure.string :as str]))
;; Generates acronym for a given string. Eg Ruby on Rails => ROR
;; Also works for camelCase, PascalCase, snake_case, kebab-case and mix of all.
;; Examples: HyperText Mark Up Language => HTML
;;           Complementary Metal-oxide-semiconductor => CMOS
;;           TO-DO_list => TDL
;;           someCamelCase WithPascalCase AND_SNAKE-kebabCamel/Pascal@CASE => SCCWPCASKCPC
;; (whitespaces at the beginning and end of the string are trimmed.)


(defn- extract-letters [word]
  (if (empty? word)
    ""
    (let [first-char (Character/toUpperCase (first word))
          rest-of-word (subs word 1)
          word (->> (cons first-char rest-of-word)
                    (apply str))]
      (->> (re-seq #"[A-Z]+" word)
           (map first)
           (apply str)))))

(defn acronym
  "Converts phrase to its acronym."
  [phrase]
  (let [words (str/split phrase #"\W")]
    (->> words
         (map extract-letters)
         (apply str))))
