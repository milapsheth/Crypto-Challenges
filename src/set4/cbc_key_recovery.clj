(ns set4.cbc-key-recovery
  (:require [set2.break-ecb-simple :as ecb]
            [util.random :as rand]
            [util.tools :as u]))


(defn cbc-key-recovery
  "Recover CBC key when used as IV"
  [encryption-oracle decryption-oracle]

  (def block-size (ecb/find-block-size encryption-oracle))

  (let [plaintext (rand/byte-lst (* 3 block-size))
        ciphertext (encryption-oracle plaintext)]
    (try
      (decryption-oracle (concat (take block-size ciphertext)
                                 (repeat block-size 0)
                                 (u/subseq' ciphertext (* 2 block-size) block-size)))

      (catch Exception e
        (let [new-plaintext (u/str->bytes (.getMessage e))]
          (u/xor (take block-size new-plaintext)
                 (u/subseq' new-plaintext (* 2 block-size) block-size)))))))
