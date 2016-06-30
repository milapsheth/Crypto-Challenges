(ns set2.cbc-attack
  (:require [set1.aes :as aes]
            [util.random :as rand]
            [clojure.string :as str]
            [set2.break-ecb-harder :as break-ecb]
            [set2.break-ecb-simple :refer [find-suffix-len find-block-size]]
            [set2.aes-oracle :as oracle]
            [util.conv :as u]))


(def random-cipher-key (rand/byte-lst 16))

(def random-iv (rand/byte-lst 16))


(defn encrypt-cookie
  [userdata]
  (when (some #(or (= \; %) (= \= %)) userdata)
    (throw (Exception. "Invalid userdata. Should not contain ; or =")))
  
  (aes/encrypt (map int (concat "comment1=cooking%20MCs;userdata="
                                userdata
                                ";comment2=%20like%20a%20pound%20of%20bacon"))
               random-cipher-key
               :cbc random-iv))


(defn decrypt-cookie
  [ciphertext]
  (u/bytes-to-str (aes/decrypt ciphertext random-cipher-key :cbc random-iv)))


(defn parse-cookie
  [cookie]
  (reduce #(conj %1 (str/split %2 #"=")) {}
          (filter #(not= % "") (str/split cookie #";"))))


(defn find-prefix-len
  "Find length of prefix attached to ciphertext"
  [block-size oracle]
  (count "comment1=cooking%20MCs;userdata="))


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
  
  (let [prefix-length (find-prefix-len oracle block-size)
        suffix-length (- (find-suffix-len block-size oracle) prefix-length)
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
