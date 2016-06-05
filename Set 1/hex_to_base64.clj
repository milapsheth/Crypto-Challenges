(ns set1.hex-to-base64)


;; CHALLENGE 1

;; Convert hex to base64

;; 49276d206b696c6c696e6720796f757220627261696e206c696b65206120706f69736f6e6f7573206d757368726f6f6d

;; =>

;; SSdtIGtpbGxpbmcgeW91ciBicmFpbiBsaWtlIGEgcG9pc29ub3VzIG11c2hyb29t



(def inp "49276d206b696c6c696e6720796f757220627261696e206c696b65206120706f69736f6e6f7573206d757368726f6f6d")

(def out "SSdtIGtpbGxpbmcgeW91ciBicmFpbiBsaWtlIGEgcG9pc29ub3VzIG11c2hyb29t")


(def hex-to-nibble (into {} (map #(-> [%1 %2])
                                 '(\0 \1 \2 \3 \4 \5 \6 \7 \8 \9 \a \b \c \d \e \f)
                                 (range 16))))

(defn byte-to-base64
  [num]
  (cond
      (< num 26) (char (+ 65 num))
      (< num 52) (char (+ 71 num))
      (< num 62) (char (+ -4 num))
      (= num 62) \+
      (= num 63) \/))

;; Block size - 24 bits
(def BLOCK_SIZE 24)
(def HEX_PER_BLOCK (/ BLOCK_SIZE 4))

(defn chop-block
  ([block] (chop-block block nil))
  ([block acc] (if (zero? block) acc (recur (bit-shift-right block 6)
                                            (cons (bit-and block 63) acc)))))

(defn encode-block
  [block]
  (let [[w x y z] (chop-block block)]
    (list (byte-to-base64 w)
          (byte-to-base64 x)
          (byte-to-base64 y)
          (byte-to-base64 z))))

(defn encode-with-padding
  [block]
  (let [[w x y z] (chop-block block)]
    (list (byte-to-base64 w)
          (byte-to-base64 x)
          (if (and (zero? z) (zero? y)) \= (byte-to-base64 y))
          (if (zero? z) \= (byte-to-base64 z)))))

(defn pad-input
  [inp]
  (map (fn [bits] (reduce #(+ (bit-shift-left %1 4) %2) 0 bits))
       (partition HEX_PER_BLOCK HEX_PER_BLOCK 0 (map hex-to-nibble (seq inp)))))

(defn encode
  [data]
  (loop [[f & rst] (pad-input data)
         acc []]
    (if (not (empty? rst))
        (recur rst (concat (reverse (encode-block f)) acc))
        (clojure.string/join (reverse (concat (reverse (encode-with-padding f)) acc))))))


(defn -main []
  (println (= out (encode inp))))
