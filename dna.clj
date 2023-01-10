(ns nucleotide-count)

;; To count the number of occurrences of a dna-base/nucleotide in a valid strand of DNA
;; Throws exception if an invalid nucleotide is provides
(defn count-of-nucleotide-in-strand [nucleotide strand] 
  {:pre [(contains? #{\A \T \C \G} nucleotide)]}
  (count (filter #(= nucleotide %) strand)))

;; To count the number of occurences of each nucleotide in a given strand of DNA
;; Returns "error" if the DNA strand provided is invalid.
(defn nucleotide-counts [strand] 
  (if (re-find #"[^ACGT]+" strand)
    "error"
    (merge {\A 0 \T 0 \C 0 \G 0} (frequencies strand))))


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
