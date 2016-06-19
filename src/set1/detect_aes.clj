(ns set1.detect-aes
  (:require [set1.aes-constants :as constants]))


(defn detect-aes
  [ciphers]
  (loop [best-match []
         best-score 0
         ciphertexts ciphers]
    (if (empty? ciphertexts)
      best-match
      (let [cipher (partition constants/BLOCK-SIZE constants/BLOCK-SIZE
                              (first ciphertexts))
            score (- (count cipher) (count (set cipher)))]
        (recur (if (> score best-score) (first ciphertexts) best-match)
               (if (> score best-score) score best-score)
               (rest ciphertexts))))))

