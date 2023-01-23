(ns protein-translation)
(defn translate-codon [codon]
  (let [lookup {"AUG" "Methionine"
                "UGG" "Tryptophan"
                "UUU" "Phenylalanine"
                "UUC" "Phenylalanine"
                "UUA" "Leucine"
                "UUG" "Leucine"
                "UCU" "Serine"
                "UCC" "Serine"
                "UCA" "Serine"
                "UCG" "Serine"
                "UAU" "Tyrosine"
                "UAC" "Tyrosine"
                "UGU" "Cysteine"
                "UGC" "Cysteine"
                "UAA" "STOP"
                "UAG" "STOP"
                "UGA" "STOP"}]
    (lookup codon)))
(defn translate-rna [rna]
  (->> (partition 3 rna)
    (reduce
     (fn [res codon]
       (let [codon (apply str codon)
             protein (translate-codon codon)]
         (if (= "STOP" protein)
           (reduced res)
           (conj res protein))))
     [])))
