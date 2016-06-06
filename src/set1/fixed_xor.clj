(ns set1.fixed-xor
  (:require [util.conv :refer :all]))


;; Challenge 2

;; Fixed XOR
;; Write a function that takes two equal-length buffers and produces their XOR combination.

;; If your function works properly, then when you feed it the string:

;; 1c0111001f010100061a024b53535009181c
;; ... after hex decoding, and when XOR'd against:

;; 686974207468652062756c6c277320657965
;; ... should produce:

;; 746865206b696420646f6e277420706c6179


(defn xor
  [data1 data2]
  (map #(bit-xor %1 %2) data1 data2))

(defn fixed-xor
  [data1 data2]
  (lst-to-str (xor (str-to-lst data1) (str-to-lst data2))))
