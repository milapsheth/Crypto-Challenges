(ns set1.repeating-xor
  (:require [util.conv :refer :all]))


;; byte[] -> byte[] -> byte[]
(defn encrypt
  "Encrypt plaintext using vigenere cipher"
  [data key]
  (loop [plaintext data
         key-pair (cycle key)
         ciphertext '()]
    (if (empty? plaintext)
      (reverse ciphertext)
      (recur (rest plaintext)
             (rest key-pair)
             (cons (bit-xor (first plaintext) (first key-pair))
                   ciphertext)))))

