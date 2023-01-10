(ns nucleotide-count)

;; To count the number of occurrences of a dna-base/nucleotide in a valid strand of DNA
;; Throws exception if an invalid nucleotide is provides
(defn count-of-nucleotide-in-strand [nucleotide strand]
  (if (or (= \A nucleotide)
          (= \C nucleotide)
          (= \G nucleotide)
          (= \T nucleotide))
    (count (filter #(= nucleotide %) strand))
    (throw (IllegalArgumentException. "error"))))

;; To count the number of occurences of each nucleotide in a given strand of DNA
;; Returns "error" if the DNA strand provided is invalid.
(defn nucleotide-counts [strand] ;; <- Arglist goes here
  ;; your code goes here
  (if (nil? (re-matches #"[^ACGT]+" strand))
    (let [a (count-of-nucleotide-in-strand \A strand)
          c (count-of-nucleotide-in-strand \C strand)
          g (count-of-nucleotide-in-strand \G strand)
          t (count-of-nucleotide-in-strand \T strand)]
      {\A a \C c \G g \T t})
    "error"))


;; ------------------------------------------------------------------------------------------

;; Alternate implementation (commented out)

(comment
(defn nucleotide-counts [strand]
  {:pre [(nil? (re-matches #"[^ACGT]+" strand))]}
  (merge {\A 0 \T 0 \C 0 \G 0} (frequencies strand)))

(defn count-of-nucleotide-in-strand [nucleotide strand]
  {:pre [(contains? #{\A \T \C \G} nucleotide)]}
  ((nucleotide-counts strand) nucleotide))
)
