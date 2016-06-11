(ns set1.detect-xor
  (:require [util.conv :refer :all]
            [clojure.string :as s]
            [set1.xor-cipher :as x]
            [clojure.java.io :as io]))


(defn detect-encrypted
  "Detect encrypted file"
  [ciphertexts]
  (loop [lines ciphertexts
         score MAX-INT
         match []
         decrypted-match '()]
    (if (empty? lines)
      (s/join decrypted-match)
      (let [data (hexstr-to-str (first lines))
            new-match (x/decrypt (first lines))
            new-score (x/score-match (seq new-match))]
        (recur (rest lines)
               (if (< new-score score) new-score score)
               (if (< new-score score) data match)
               (if (< new-score score) new-match decrypted-match))))))

