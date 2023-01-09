(ns log-levels
  (:require [clojure.string :as str]))

(defn- parse-log [s]  ;; function to parese the log-line into (level , message) using a regex
  (let [regex #"\[(.*)\]: (.*)"]
    (->> (re-find regex s)
         (map str/trim)
         rest)))

(defn message ;; extracts message from the log-line
  "Takes a string representing a log line
   and returns its message with whitespace trimmed."
  [s]
  (-> s parse-log second))

(defn log-level ;; extracts the level of the message in the log-line (and also converts it into lower-case)
  "Takes a string representing a log line
   and returns its level in lower-case."
  [s]
  (-> s parse-log first str/lower-case))
