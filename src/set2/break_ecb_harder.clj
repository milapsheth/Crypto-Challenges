(ns set2.break-ecb-harder)


(defn get-random-prefix-length
  "Find the length of random prefix"
  [oracle]
  )

(defn break-ecb
  "Break AES(ECB) encryption using chosen 
  plaintext attack to find unknown string"
  [oracle]
  
  (def block-size (discover-block-size oracle))
  
  (let [ciphertext (oracle (repeat (* 4 block-size) (int \A)))]
    (when-not (= :ecb (detect-mode ciphertext))
      (throw (Exception. "Encryption mode is not ECB and cannot be broken(yet)"))))

  (def prefix-length (get-random-prefix-length oracle))
  
  (def unknown-str-len (get-unknown-string-len block-size oracle))

  (loop [found-bytes []
         text (vec (repeat block-size (int \A)))]

    (if (>= (count found-bytes) unknown-str-len)

      found-bytes

      (let [new-text (subvec text 1)
            next-byte (discover-next-byte new-text (count found-bytes) block-size oracle)]
        
        (recur (conj found-bytes next-byte)
               (conj new-text next-byte))))))

