(ns set1.decrypt-vigenere
  (:require [util.conv :as u]
            [clojure.string :as s]
            [set1.xor-cipher :as x]
            [clojure.java.io :as io]))


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
  (loop [key-sizes (range 2 40)
         best-key '()
         best-distance u/MAX-INT]
    (if (or (empty? key-sizes) (> (first key-sizes) len))
      best-key
      (let [key-size (first key-sizes)
            edit-distance (calc-edit-distance data key-size)]
        (recur (rest key-sizes)
               (cons [key-size edit-distance] best-key) #_(if (< edit-distance best-distance) key-size best-key)
               (if (< edit-distance best-distance) edit-distance best-distance))))))


(defn decrypt-vigenere
  [data]
  (def key-sizes (get-good-key-sizes data))
  ;; (println key-sizes)
  (loop [keys (take 4 (sort-by second < key-sizes))
         acc '()]
    (if (empty? keys)
      (second (reduce #(let [score (x/score-match %2)]
                         (if (< score (%1 0)) [score %2] %))
                      [u/MAX-INT nil] acc))
      (let [[key-len score] (first keys)
            cipher data]
        ;; (println score)
        (recur (rest keys)
               (cons (apply interleave (map #(x/decrypt-caesar
                                              (take-nth key-len (drop % cipher))) #_ (map (fn [block] (nth block % nil)) cipher)
                                            (range key-len)))
                     acc))))))


(defn read-data []
  (with-open [rdr (io/reader (io/file (io/resource "6.txt")))]
    (let [data (u/base64-to-byte' (apply concat (line-seq rdr)))]
      #_(decrypt-vigenere data) (s/join (map char (decrypt-vigenere data))) #_
      (map #(s/join (map char %)) (decrypt-vigenere data)))))

;(decrypt-vigenere (u/base64-to-byte' "aRedtBf6d+4="))
;(read-data)
