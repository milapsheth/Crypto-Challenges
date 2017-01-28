(ns set1.decrypt-vigenere
  (:require [clojure.java.io :as io]
            [clojure.string :as s]
            [set1.xor-cipher :as x]
            [util.tools :as u]))

(def MAX-KEY-LEN 40)

(defn count-set-bits [n]
  (loop [n n
         acc 0]
    (if (zero? n)
      acc
      (recur (quot n 2) (+ (rem n 2) acc)))))


(defn hamming-distance
  [str1 str2]
  (reduce + 0 (map #(count-set-bits (bit-xor (int %1) (int %2))) str1 str2)))

(defn calc-edit-distance
  [data key-size]
  (/ (reduce
      #(+ %1 (hamming-distance (drop (* %2 key-size) (take (* (inc %2) key-size) data))
                               (drop (* (inc %2) key-size) (take (* (+ 2 %2) key-size) data))))
      0 (range 4))
     (* 4 key-size)))


(defn get-good-key-sizes
  [data]
  (def len (quot (count data) 10))
  (loop [key-sizes (range 2 (min (inc MAX-KEY-LEN) len))
         best-key '()
         best-distance u/MAX-NUM]
    (if (or (empty? key-sizes) (> (first key-sizes) len))
      best-key
      (let [key-size (first key-sizes)
            edit-distance (calc-edit-distance data key-size)]
        (recur (rest key-sizes)
               (cons [key-size edit-distance] best-key)
               (if (< edit-distance best-distance) edit-distance best-distance))))))


(defn decrypt-vigenere
  [cipher]
  (def key-sizes (get-good-key-sizes cipher))
  (loop [keys (take 4 (sort-by second < key-sizes))
         acc '()]
    (if (empty? keys)
      (second (reduce #(let [text-score (x/score-match %2)]
                         (if (< text-score (%1 0)) [text-score %2] %))
                      [u/MAX-NUM nil] acc))
      (let [[key-len cipher-score] (first keys)]
        (recur (rest keys)
               (cons (apply u/interleave' (map #(x/decrypt-caesar
                                                 (take-nth key-len (drop % cipher)))
                                             (range key-len)))
                     acc))))))

