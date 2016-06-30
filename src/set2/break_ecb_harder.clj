(ns set2.break-ecb-harder
  (:require [set1.aes :as aes]
            [set2.break-ecb-simple :as ecb]
            [clojure.string :as str]
            [util.conv :as u]
            [set2.aes-oracle :as oracle]))


(defn get-equal-block-index
  "Return the index of the first consecutive equal blocks"
  [ciphertext]
  (loop [blocks (rest ciphertext)
         i 1 block-num 0
         last-block (first ciphertext)
         equal-count 1]
    (cond
      (> equal-count 2) {:index block-num, :block last-block}
      (empty? blocks) (throw (Exception. "Couldn't find equal consecutive block"))
      :else (let [equals? (= last-block (first blocks))]
              (recur (rest blocks)
                     (inc i)
                     (if equals? block-num i)
                     (if equals? last-block (first blocks))
                     (if equals? (inc equal-count) 1))))))


(defn find-prefix-len
  "Find the length of random prefix"
  [oracle block-size]
  (def empty-cipher (oracle '()))
  (def empty-cipher-len (count empty-cipher))
  (def known-block (get-equal-block-index (u/partition' block-size
                                                      (oracle (repeat (* 4 block-size) 0)))))
  
  (loop [i 0
         text '(0)]
    (when (> i 100)
      (throw (Exception. "Random prefix is too long")))
    (let [cipher (oracle text)
          idx (u/find-seq cipher (known-block :block))]

      (if (and idx (= idx (* block-size (known-block :index))))
        (- idx (- i block-size) 1)
        (recur (inc i)
               (cons 0 text))))))


(defn discover-next-byte
  "Find next byte of unknown string"
  [known-text obtained-bytes block-size prefix-padding prefix-block oracle]
  (let [block-to-attack (* block-size (quot obtained-bytes block-size))
        ciphertext (take block-size (drop block-to-attack
                                          (oracle (repeat (+ prefix-padding
                                                             (- (dec block-size) (rem obtained-bytes block-size))) 0))))
        plaintext (vec known-text)]
    
    (loop [chr 0]
      (if (= chr 256)
        (throw (Exception. "Couldn't find the next byte"))
        (let [cipher-block (take block-size (drop prefix-block (oracle (conj plaintext chr))))]
          (if (= ciphertext cipher-block)
            chr
            (recur (inc chr))))))))


(defn break-ecb
  "Break AES(ECB) encryption using chosen 
  plaintext attack to find unknown string"
  [oracle]
  
  (def block-size (ecb/find-block-size oracle))

  (let [ciphertext (oracle (repeat (* 4 block-size) 0))]
    (when-not (= :ecb (oracle/detect-mode ciphertext))
      (throw (Exception. "Encryption mode is not ECB and cannot be broken(yet)"))))

  (let [prefix-length (find-prefix-len oracle block-size)

        unknown-str-len (- (ecb/find-suffix-len block-size oracle) prefix-length)

        prefix-padding (- block-size (rem prefix-length block-size))
        
        prefix-block (+ prefix-padding prefix-length)]

    (loop [found-bytes []
           byte-index (+ (- prefix-length (rem prefix-length block-size))
                         block-size)
           text (vec (repeat (- (* 2 block-size) (rem prefix-length block-size)) 0))]

      (if (>= (count found-bytes) unknown-str-len)

        found-bytes

        (let [new-text (subvec text 1)
              next-byte (discover-next-byte new-text
                                            byte-index
                                            block-size
                                            prefix-padding
                                            prefix-block
                                            oracle)]
          
          (recur (conj found-bytes next-byte)
                 (inc byte-index)
                 (conj new-text next-byte)))))))

