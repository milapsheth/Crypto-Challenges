(ns set1.xor-cipher
  (:require [set1.fixed-xor :refer :all]
            [util.conv :refer :all]))


;; Challenge 3

;; Single-byte XOR cipher
;; The hex encoded string:

;; 1b37373331363f78151b7f2b783431333d78397828372d363c78373e783a393b3736
;; ... has been XOR'd against a single character. Find the key, decrypt the message.

;; You can do this by hand. But don't: write code to do it for you.

;; How? Devise some method for "scoring" a piece of English plaintext. Character frequency is a good metric. Evaluate each output and choose the one with the best score.


;; 12 most frequently occuring charaters in English
(def char-freq [\e \t \a \o \space \i \n \s \h \r \d \l \u])


(defn score-match
  [match]
  (def char-count (reduce #(update %1 %2 (fnil inc 0)) {} match))
  (reduce' #(+ %1 (* %3 (get char-count %2 0))) 0
           char-freq (range 26 13 -1)))

(defn rotate-by-char
  [byte-lst byte]
  (map #(char (bit-xor % byte)) byte-lst))

;; Input hex string "1b4f36a" -> decrypted ascii
(defn brute-force
  "Brute force through all rotations and output the one with best score"
  [cdata]
  (def data (map int (hexstr-to-str cdata)))
  (loop [score 0
         matched-data (map char data)
         nums (range 256)]
    
    (if (empty? nums)
      (clojure.string/join matched-data)
      (let [match (rotate-by-char data (first nums))
             new-score (score-match match)]
        (recur (if (> new-score score) new-score score)
               (if (> new-score score) match matched-data)
               (rest nums))))))

(defn decrypt
  "Get most frequent character and then assuming
  it is one of the most frequent character, 
  output the data with best score"
  [cdata]
  (def data (map int (hexstr-to-str cdata)))
  (def char-count (reduce #(update %1 %2 (fnil inc 0)) {} data))

  (let [max-char (key (apply max-key val char-count))]
    (loop [freq-lst char-freq
           best-score 0
           best-match data]

      (if (empty? freq-lst)
        (clojure.string/join best-match)

        (let [cipher-key (bit-xor max-char (int (first freq-lst)))
              match (rotate-by-char data cipher-key)
              score (score-match match)]
          (recur (rest freq-lst)
                 (if (> score best-score) score best-score)
                 (if (> score best-score) match best-match)))))))
