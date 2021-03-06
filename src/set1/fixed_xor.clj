(ns set1.fixed-xor
  (:require [util.tools :as u]))

;; Challenge 2

;; Fixed XOR
;; Write a function that takes two equal-length buffers and produces their XOR combination.

;; If your function works properly, then when you feed it the string:

;; 1c0111001f010100061a024b53535009181c
;; ... after hex decoding, and when XOR'd against:

;; 686974207468652062756c6c277320657965
;; ... should produce:

;; 746865206b696420646f6e277420706c6179


(defn fixed-xor
  [data1 data2]
  (clojure.string/join (map u/int->hex (u/xor (u/str->lst data1) (u/str->lst data2)))))
