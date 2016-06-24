(ns set2.aes-oracle
  (:require [set1.aes :as aes]
            [set1.detect-aes :as detect]
            [util.random :as rand]))

(defn encrypt
  "Oracle encryption function.
  Returns AES encryption under ECB or CBC mode"
  [plaintext]
  (let [padded-text (concat (rand/bytes (+ 5 (rand-int 6)))
                            plaintext
                            (rand/bytes (+ 5 (rand-int 6))))
        cipher-key (repeatedly 16 #(rand-int 256))
        choice (rand-int 2)]
    [([:ecb :cbc] choice),
     (if (zero? choice)
       (aes/encrypt plaintext cipher-key :ecb)
       (let [iv (repeatedly 16 #(rand-int 256))]
         (aes/encrypt plaintext cipher-key :cbc iv)))]))


(defn detect-mode
  "Detect AES encryption mode"
  [ciphertext]
  ;; If score is greater than 1, then ECB mode, else CBC
  ([:ecb :cbc] (if (< 1 ((detect/detect-aes [ciphertext]) 1))
                 0
                 1)))
