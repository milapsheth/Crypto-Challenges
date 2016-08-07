(ns set3.break-ctr
  (:require [set1.xor-cipher :as caesar]
            [util.tools :as u]))


(defn encode-ciphers
  [max-len ciphers]
  (map #(u/option-map' (fn [[i c]] (some->> (get c % nil) (vector i)))
                       (map vector (range) ciphers))
       (range max-len)))


(defn decode-ciphers
  [indexes ciphers]
  (reduce (fn [mp [idx cipher]]
            (reduce (fn [v [i c]]
                      (update v i #(conj % c)))
                    mp
                    (map vector idx cipher)))
          (vec (repeat (count (apply max-key count ciphers)) []))
          (map vector indexes ciphers)))


(defn break-ctr
  "Break CTR encrypted ciphertext statistically"
  [ciphertexts]
  (let [ciphers (map vec ciphertexts)
        max-len (count (apply max-key count ciphers))
        caesar-ciphers (encode-ciphers max-len ciphers)]
    
    caesar-ciphers
    (->> (map #(mapv second %) caesar-ciphers)
         (map caesar/decrypt-caesar)
         (decode-ciphers (map #(mapv first %) caesar-ciphers)))))
