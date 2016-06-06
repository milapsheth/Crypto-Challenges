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
(def char-freq [\e \t \a \o \i \n \s \h \r \d \l \u])


(defn score-match
  [match]
  (def char-count (reduce #(update %1 %2 (fnil inc 0)) {} match))
  (reduce' #(+ %1 (* %3 (get char-count %2 0))) 0
           char-freq (range 26 14 -1)))

;; Input hex string "1b4f36a" -> decrypted ascii
(defn brute-force
  [cdata]
  (def data (map int (hexstr-to-str cdata)))
  (loop [score 0
         matched-data (map char data)
         nums (range 256)]
    
    (if (empty? nums)
      (clojure.string/join matched-data)
      (let [match (map #(char (bit-xor % (first nums))) data)
             new-score (score-match match)]
        (recur (if (> new-score score) new-score score)
               (if (> new-score score) match matched-data)
               (rest nums))))))

(defn decrypt
  [data]
  (def char-count (reduce #(update-in %1 [%2] (fnil inc 0)) (seq data)))
  )
