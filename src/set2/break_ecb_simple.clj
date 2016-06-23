(ns set2.break-ecb-simple
  (:require [set1.aes :as aes]
            [set2.aes-oracle :refer [detect-mode]]))


(def MAX-BLOCK-SIZE 32)

(defn discover-block-size
  [oracle]
  (loop [plaintext '(65)
         last-block-size 0
         blocks #{}]
    
    (if (> last-block-size MAX-BLOCK-SIZE)
      (throw (Exception. "Couldn't discover block size"))
      (let [encrypted-block (oracle plaintext)]
        ;; Test it on 2 consecutive lengths of ciphertext to protect against any collisions
        (if (and (blocks (take last-block-size encrypted-block))
                 (blocks (take last-block-size (oracle (cons 65 plaintext)))))
          last-block-size
          (recur (cons 65 plaintext)
                 (inc last-block-size)
                 (conj blocks (take (inc last-block-size) encrypted-block))))))))


(defn get-unknown-string-len
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


(defn create-plaintext-for-attack
  "Creates plaintext padded n * block-size - 1
  to attack ECB mode encryption"
  [previous-text block-size]
  (let [len (count previous-text)]
    (vec (concat (repeat (- (dec block-size) (rem len block-size)) 65)
                 previous-text))))


(defn break-ecb
  "Break AES(ECB) encryption using chosen 
  plaintext attack to find unknown string"
  [oracle]
  
  (def block-size (discover-block-size oracle))
  
  (let [ciphertext (oracle (repeat (* 3 block-size) (int \A)))]
    (when-not (= :ecb (detect-mode ciphertext))
      (throw (Exception. "Encryption mode is not ECB and cannot be broken(yet)"))))

  (def unknown-str-len (get-unknown-string-len block-size oracle))

  (loop [found-bytes []
         text (vec (repeat block-size (int \A)))]

    (if (>= (count found-bytes) unknown-str-len)

      found-bytes

      (let [new-text (subvec text 1)
            next-byte (discover-next-byte new-text (count found-bytes) block-size oracle)]
        
        (recur (conj found-bytes next-byte)
               (conj new-text next-byte))))))

