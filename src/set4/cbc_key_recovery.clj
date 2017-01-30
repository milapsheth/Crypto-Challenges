(ns set4.cbc-key-recovery
  (:require [set2.break-ecb-simple :as ecb]
            [util.random :as rand]
            [util.tools :as u]
            [set2.cbc-attack :as cbc-attack]
            util.CryptoException))


(defn create-malicious-ciphertext
  "Create malicious ciphertext to be sent for decryption"
  [prefix-len block-size oracle]

  (let [plaintext-prefix-len (mod (- 0 prefix-len) block-size)
        plaintext-prefix (repeat plaintext-prefix-len 65)
        plaintext (concat plaintext-prefix
                          (u/str->bytes (rand/string (* 3 block-size))))
        ;; plaintext-for-modify (concat plaintext-prefix (u/str->bytes (rand/string block-size)))
        ;; block-prefix-len (+ prefix-len plaintext-prefix-len)
        ciphertext (oracle plaintext)
        ;; ciphertext-for-modify (oracle plaintext-for-modify)
        ]

    (->> ciphertext
         #_(drop block-prefix-len)
         (take block-size)
         (#(concat #_(take block-prefix-len ciphertext)
                   % (repeat block-size 0) %
                   (drop (+ 0 #_block-prefix-len (* 3 block-size)) ciphertext))))))


(defn cbc-key-recovery
  "Recover CBC key when used as IV"
  [encryption-oracle decryption-oracle]

  (def block-size (ecb/find-block-size encryption-oracle))
  (def prefix-len (cbc-attack/find-prefix-len block-size encryption-oracle))

  (try
    (dotimes [n 30]
      (let [ciphertext (create-malicious-ciphertext prefix-len block-size encryption-oracle)]

        ;; (try
          (println (u/bytes->str (decryption-oracle ciphertext)))
          ;; (catch Exception e
            ;; (when-not (= "Invalid padding" (.getMessage e))
     #_         (throw e)))

    (catch util.CryptoException e
      (let [new-plaintext (u/str->bytes (.getMessage e))]
        ;; new-plaintext #_
        (u/xor (take block-size new-plaintext)
               #_ (drop (* 2 block-size) new-plaintext)
               (u/subseq' new-plaintext (* 2 block-size) block-size))))))
