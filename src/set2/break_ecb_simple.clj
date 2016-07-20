(ns set2.break-ecb-simple
  (:require [set2.aes-oracle :refer [detect-mode]]
            [util.aes :as aes]))

(def MAX-BLOCK-SIZE 32)

(defn find-block-size
  [oracle]
  (def empty-cipher-length (count (oracle '())))
  (loop [plaintext '(0)
         text-size 0]
    
    (if (> text-size MAX-BLOCK-SIZE)
      (throw (Exception. "Couldn't discover block size"))
      (let [cipher-length (count (oracle plaintext))]

        (if (> cipher-length empty-cipher-length)
          (- cipher-length empty-cipher-length)
          (recur (cons 0 plaintext)
                 (inc text-size)))))))


(defn find-suffix-len
  "Get the length of unknown string
  Checks the maximum size of ciphertext
  for encoding text of length 0 to block-size - 1
  and deducts the size of string"
  [block-size oracle]
  (- (apply - (reduce (fn [[len pad] i] (let [cipher-len (count (oracle (repeat i 65)))]
                                          (if (> cipher-len len)
                                            [cipher-len i]
                                            [len pad])))
                      [0 0]
                      (range block-size)))
     block-size))


(defn discover-next-byte
  "Find next byte of unknown string"
  [known-text obtained-bytes block-size oracle]
  (let [block-to-attack (quot obtained-bytes block-size)
        ciphertext (take block-size (drop (* block-to-attack block-size)
                                          (oracle (repeat (- (dec block-size) (rem obtained-bytes block-size)) (int \A)))))
        plaintext (vec known-text)]

    (loop [chr 0]
      (if (= chr 256)
        (throw (Exception. "Couldn't find the next byte"))
        (let [cipher-block (take block-size (oracle (conj plaintext chr)))]
          (if (= ciphertext cipher-block)
            chr
            (recur (inc chr))))))))


(defn break-ecb
  "Break AES(ECB) encryption using chosen 
  plaintext attack to find unknown string"
  [oracle]
  
  (def block-size (find-block-size oracle))
  
  (let [ciphertext (oracle (repeat (* 3 block-size) (int \A)))]
    (when-not (= :ecb (detect-mode ciphertext))
      (throw (Exception. "Encryption mode is not ECB and cannot be broken(yet)"))))

  (def unknown-str-len (find-suffix-len block-size oracle))

  (loop [found-bytes []
         text (vec (repeat block-size (int \A)))]

    (if (>= (count found-bytes) unknown-str-len)

      found-bytes

      (let [new-text (subvec text 1)
            next-byte (discover-next-byte new-text (count found-bytes) block-size oracle)]
        
        (recur (conj found-bytes next-byte)
               (conj new-text next-byte))))))

