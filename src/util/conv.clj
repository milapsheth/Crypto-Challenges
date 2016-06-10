(ns util.conv
  (:require [clojure.string :as s]))


(defn hex-to-int
  [ch]
  (let [c (int ch)]
    (if (< c 97)
      (- c 48)
      (- c 87))))

(defn int-to-hex
  [c]
  (char (+ c (if (< c 10) 48 87))))

(defn sqr [n] (* n n))

(def MAX-INT (bit-shift-left 1 48))

(defn hexstr-to-str
  [data]
  (mapv #(char (+ (bit-shift-left (hex-to-int (first %)) 4) (hex-to-int (second %))))
        (partition 2 2 0 (seq data))))

(defn down-case
  [c]
  (let [ch (int c)]
    (if (and (< ch 91) (> ch 64))
      (char (+ ch 32))
      c)))

(defn str-to-lst
  [data]
  (mapv hex-to-int (seq data)))

(defn lst-to-str [data] (clojure.string/join (map int-to-hex data)))

(defn reduce'
  [f b l & coll]
  (if (empty? l)
    b
    (loop [lst (apply map vector (cons l coll))
           acc b]
      (if (empty? lst)
        acc
        (recur (rest lst) (apply f acc (first lst)))))))


;; Base 64 to byte array

(defn base64-to-bits
  [ch]
  (let [c (int ch)]
    (cond
      (and (> c 64) (< c 91)) (- c 65)
      (and (> c 96) (< c 123)) (- c 71)
      (and (> c 47) (< c 58)) (+ c 4)
      (= c 43) 62
      (= c 47) 63
      (= c 61) 0)))

(defn transform-block
  [block]
  (loop [block (reduce #(+ %2 (bit-shift-left %1 6)) block)
         n 3
         acc []]
    (if (zero? n)
      acc
      (recur (bit-shift-right block 8)
             (dec n)
             (cons (bit-and block 255) acc)))))

(defn decode-base64-block
  [block]
  (transform-block block))

(defn decode-base64-with-padding
  [[w x y z]]
  (cond
    (and (zero? z) (zero? y)) (list (+ (bit-shift-left w 2) (bit-shift-right x 4)))
    (zero? z) (list (+ (bit-shift-left w 2) (bit-shift-right x 4))
                    (+ (bit-and 255 (bit-shift-left x 4)) (bit-shift-right y 2)))
    :else (transform-block [w x y z])))

(defn base64-to-byte'
  "Convert base64 string to byte array"
  [data]
  (loop [[block & rst] (partition 4 4 (map base64-to-bits data))
         acc []]
    (if (empty? rst)
      (reverse (concat (reverse (decode-base64-with-padding block)) acc))
      (recur rst (concat (reverse (decode-base64-block block)) acc)))))
