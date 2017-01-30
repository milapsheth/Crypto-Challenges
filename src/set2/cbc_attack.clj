(ns set2.cbc-attack
  (:require [set2
             [aes-oracle :as oracle]
             [break-ecb-simple :refer [find-block-size]]]
            [util
             [random :as rand]
             [tools :as u]]))


(defn get-first-unequal-block-index
  "Get the index of the first block that is unequal"
  [bytes1 bytes2 block-size]
  (loop [[block1 & rst1] (u/partition' block-size bytes1)
         [block2 & rst2] (u/partition' block-size bytes2)
         i 0]
    (cond
      (= block1 block2) (if (or (nil? rst1)
                                (nil? rst2))
                          (u/raise "Both byte arrays are same: "
                                   bytes1 "\n" bytes2)
                          (recur rst1 rst2 (inc i)))
      :else i)))


(defn find-prefix-len
  "Find length of prefix attached to ciphertext"
  [block-size oracle]
  (let [unequal-index (get-first-unequal-block-index (oracle [])
                                                     (oracle [65])
                                                     block-size)]
    (loop [i 1]
      (if (> i block-size)
        (u/raise "Could not find prefix length")

        (let [index (get-first-unequal-block-index (oracle (repeat i 65))
                                                   (oracle (repeat (inc i) 65))
                                                   block-size)]
          (cond (= index unequal-index) (recur (inc i))
                :else (- (* index block-size) i)))))))  
  

(defn find-prefix-suffix-len
  "Find the total length of prefix and suffix
  during encryption"
  [block-size oracle]
  (let [encrypted-prefix-len (count (oracle []))]
    (loop [i 1]
      (if (> i block-size)
        (u/raise "Could not find prefix length")

        (let [encrypted-plaintext-len (count (oracle (repeat i 0)))]

          (cond
            (> encrypted-plaintext-len encrypted-prefix-len) (- encrypted-prefix-len i)
            :else (recur (inc i))))))))


(defn create-malicious-ciphertext
  "Creates a block to break CBC"
  [block-size valid-block?]
  (loop [i 10]
    (if (zero? i)
      (throw (Exception. "Couldn't create a valid block"))

      (let [block (rand/string block-size)]
        (if (valid-block? block)
          block
          (recur (dec i)))))))


(defn create-malicious-text-block
  "Create malicious plaintext to encrypt"
  [prefix-padding previous-cipher plaintext block-num block-size oracle]
  (fn [block]
    (->> (u/subseq' (oracle (concat prefix-padding block)) block-num block-size)
         (#(u/xor % previous-cipher plaintext)))))


(defn valid-attack-block?
  "Validate if a created block can be encrypted"
  [create-malicious-block]
  (let [chr1 (int \;) chr2 (int \=)]
    (fn [block]
      (and (not (some #(or (= % chr1) (= % chr2)) block))
           (->> (create-malicious-block block)
                (some #(or (= % chr1) (= % chr2)))
                (not))))))


(defn attack-cbc
  "Break CBC by injecting malicious content"
  [user-text oracle]

  (def block-size (find-block-size oracle))

  (when (> (count user-text) block-size)
    (throw (Exception. "User data above block-size is not yet supported")))

  (let [ciphertext (oracle (repeat (* 4 block-size) 0))]
    (when-not (= :cbc (oracle/detect-mode ciphertext))
      (throw (Exception. "Encryption mode is not CBC"))))
  
  (let [prefix-length (find-prefix-len block-size oracle)
        suffix-length (- (find-prefix-suffix-len block-size oracle) prefix-length)
        cipher-block-num (* block-size (quot prefix-length block-size))]

    (let [prefix-padding (repeat (- block-size (rem prefix-length block-size)) 65)

          padded-data (concat (repeat (- block-size (rem (count user-text) block-size)) 65)
                              (map int user-text))

          empty-cipher (oracle prefix-padding)

          fake-block-creator (create-malicious-text-block prefix-padding
                                                     (u/subseq' empty-cipher cipher-block-num block-size)
                                                     padded-data
                                                     (+ block-size cipher-block-num)
                                                     block-size
                                                     oracle)
          attack-block (create-malicious-ciphertext block-size (valid-attack-block? fake-block-creator))]
      
      (->> (fake-block-creator attack-block)
           (concat prefix-padding attack-block)
           (oracle)
           (drop (+ (* 2 block-size) cipher-block-num))
           (concat (take (+ block-size cipher-block-num) empty-cipher))
           )))
  )
