(ns set3.cbc-padding-oracle
  (:require [set1.aes :as aes]
            [util.conv :as u]
            [util.random :as rand]
            [set1.padding :as padding]))


(defn find-last-byte
  "Find the last byte of plaintext."
  [ciphertext byte-to-attack found-bytes block-size padding-oracle]

  (let [num-byte (rem byte-to-attack block-size) ;; Byte index to start modifying
        padding-byte (- block-size num-byte)
        [remaining-text text-being-attacked] (split-at (* block-size (dec (quot byte-to-attack block-size)))
                                                        ciphertext)
        block-to-attack (take block-size text-being-attacked)
        affected-block (u/subseq' text-being-attacked block-size block-size)]

    (loop [guess-byte 0]
      (if (> guess-byte 255)
        (throw (Exception. "Couldn't find byte"))
        
        (let [modified-block (concat (repeat num-byte 255) ;; Rest of ciphertext is useless; fill it with MAX-BYTE elements
                                     (->> block-to-attack
                                          (drop num-byte)
                                          (map #(bit-xor %1 %2 padding-byte)
                                               (cons guess-byte found-bytes))))
              modified-cipher (concat remaining-text modified-block affected-block)]
          (if (padding-oracle (take block-size modified-cipher)
                              (drop block-size modified-cipher))
            guess-byte
            (recur (inc guess-byte))))))))


(defn cbc-padding-oracle-attack
  "Padding oracle attack on CBC"
  [iv cipher block-size padding-oracle]

  (let [ciphertext (concat iv cipher)] ;; Treat iv as first block of cipher to decrypt the actual first block of cipher
    (-> (loop [found-bytes '()
               byte-to-attack (dec (count ciphertext))]

          (if (< byte-to-attack block-size)
            found-bytes
            (recur (cons (find-last-byte ciphertext byte-to-attack found-bytes block-size padding-oracle)
                         found-bytes)
                   (dec byte-to-attack))))
        (padding/unpad-plaintext block-size :pkcs7))))
