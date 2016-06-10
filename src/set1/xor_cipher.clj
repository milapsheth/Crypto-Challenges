(ns set1.xor-cipher
  (:require [set1.fixed-xor :refer :all]
            [util.conv :refer :all]))


;; Challenge 3

;; Single-byte XOR cipher
;; The hex encoded string:

;; 1b37373331363f78151b7f2b783431333d78397828372d363c78373e783a393b3736
;; ... has been XOR'd against a single character. Find the key, decrypt the message.

;; You can do this by hand. But do not: write code to do it for you.

;; How? Devise some method for "scoring" a piece of English plaintext. Character frequency is a good metric. Evaluate each output and choose the one with the best score.


;; English letters in decreasing order of frequency
(def char-freq-map {\e 127, \t 90, \a 81, \o 75, \i 70, \n 67, \s 63,
                    \h 60, \r 59, \d 42, \l 40, \c 27, \u 26, \m 24,
                    \w 23, \f 22, \g 20, \y 19, \p 18, \b 15, \v 10,
                    \k 8, \j 2, \x 1, \q 1, \z 1,
                    \space 200, \. 32, \, 31, \; 3, \: 4, \! 3, \? 6,
                    \' 24, \newline 35})

(def char-freq (map key (sort-by val > char-freq-map)))
(def char-freq-count (map val (sort-by val > char-freq-map)))

(def UNCOMMON-CHAR-WEIGHT -20)

;; Score = Σ (ch-normal-freq - (char-count * 1000 / doc-length))
;; TODO: Make 1000 into variable percent based on doc-length
(defn score-match
  "Score an english text based on how close 
  the letter distribution is to that commonly found in text"
  [match]
  (let [len (count match)
        char-count (reduce #(update %1 (char %2) (fnil inc 0)) {} match)]
    (reduce #(+ %1 (sqr (- (get char-freq-map (down-case (key %2)) UNCOMMON-CHAR-WEIGHT)
                           (/ (* 1000 (val %2)) len))))
            0 char-count)))


(defn rotate-byte-array
  [byte-lst byte]
  (map #(bit-xor % byte) byte-lst))


;; Input hex string "1b4f36a" -> decrypted ascii
(defn brute-force
  "Brute force through all rotations and output the one with best score"
  [cdata]
  (def data (map int (hexstr-to-str cdata)))
  (loop [score MAX-INT
         matched-data (map char data)
         nums (range 256)]

    (if (empty? nums)
      (clojure.string/join matched-data)
      (let [match (rotate-by-char data (first nums))
             new-score (score-match match)]
        (recur (if (< new-score score) new-score score)
               (if (< new-score score) match matched-data)
               (rest nums))))))


(defn decrypt-caesar
  "Get most frequent character and then assuming
  it is one of the most frequent character, 
  output the data with best score"
  [data]
  (def char-count (reduce #(update %1 %2 (fnil inc 0)) {} data))

  (let [max-char (key (apply max-key val char-count))]
    (loop [freq-lst (take 10 char-freq)
           best-score MAX-INT
           best-match data]

      (if (empty? freq-lst)
        best-match

        (let [cipher-key (bit-xor max-char (int (first freq-lst)))
              match (rotate-byte-array data cipher-key)
              score (score-match match)]
          (recur (rest freq-lst)
                 (if (< score best-score) score best-score)
                 (if (< score best-score) match best-match)))))))

(defn decrypt
  [data]
  (clojure.string/join (map char (decrypt-caesar (map int (hexstr-to-str data))))))
