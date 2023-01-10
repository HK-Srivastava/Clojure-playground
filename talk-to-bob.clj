;; Bob is a lackadaisical teenager. In conversation, his responses are very limited.
;; Bob answers 'Sure.' if you ask him a question, such as "How are you?".
;; He answers 'Whoa, chill out!' if you YELL AT HIM (in all capitals).
;; He answers 'Calm down, I know what I'm doing!' if you yell a question at him.
;; He says 'Fine. Be that way!' if you address him without actually saying anything.
;; He answers 'Whatever.' to anything else
;; Bob's conversational partner is a purist when it comes to written communication and always follows normal rules regarding sentence punctuation in English.

(ns bob
  (:require [clojure.string :refer [trim]]))

(defn response-for [s] 
  (let [s (trim s)
        ques? (= \? (last s))
        has-letter? (some #(Character/isLetter %) s)
        has-lower-case? (some #(Character/isLowerCase %) s)
        yelling? (and has-letter? (not has-lower-case?))]
    (cond
      (= s "") "Fine. Be that way!"
      (and yelling? ques?) "Calm down, I know what I'm doing!"
      yelling? "Whoa, chill out!"
      ques? "Sure."
      :else "Whatever.")))
