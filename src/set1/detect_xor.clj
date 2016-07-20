(ns set1.detect-xor
  (:require [clojure.java.io :as io]
            [clojure.string :as s]
            [set1.xor-cipher :as x]
            [util.tools :refer :all]))

(defn decrypt
  [data]
  (clojure.string/join (map char (x/decrypt-caesar (map int (hexstr->str data))))))


(defn detect-encrypted
  "Detect encrypted file"
  [ciphertexts]
  (loop [lines ciphertexts
         score MAX-INT
         match []
         decrypted-match '()]
    (if (empty? lines)
      (s/join decrypted-match)
      (let [data (hexstr->str (first lines))
            new-match (decrypt (first lines))
            new-score (x/score-match (seq new-match))]
        (recur (rest lines)
               (if (< new-score score) new-score score)
               (if (< new-score score) data match)
               (if (< new-score score) new-match decrypted-match))))))

