(ns set1.fixed-xor)


;; Challenge 2

;; Fixed XOR
;; Write a function that takes two equal-length buffers and produces their XOR combination.

;; If your function works properly, then when you feed it the string:

;; 1c0111001f010100061a024b53535009181c
;; ... after hex decoding, and when XOR'd against:

;; 686974207468652062756c6c277320657965
;; ... should produce:

;; 746865206b696420646f6e277420706c6179


(def inp1 "1c0111001f010100061a024b53535009181c")

(def inp2 "686974207468652062756c6c277320657965")

(def out "746865206b696420646f6e277420706c6179")


(defn hex-to-int
  [ch]
  (let [c (int ch)]
    (if (< c 97)
      (- c 48)
      (- c 87))))

(defn int-to-hex
  [c]
  (char (+ c (if (< c 10) 48 87))))

(defn xor
  [data1 data2]
  (clojure.string/join (map #(int-to-hex (bit-xor (hex-to-int %1) (hex-to-int %2)))
                            (seq data1) (seq data2))))


(defn -main
  []
  (assert (= out (xor inp1 inp2))))
