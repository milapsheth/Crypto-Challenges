(ns util.tools
  (:require [clojure.string :as str]))


;; Bit-shift shorthands
(defn << [x a] (bit-shift-left x a))

(defn >> [x a] (bit-shift-right x a))

(defn |
  "Bit-or shorthand"
  ([x y] (bit-or x y))
  ([x y & z] (apply bit-or x y z)))

(defn &
  "Bit-and shorthand"
  ([x y] (bit-and x y))
  ([x y & z] (apply bit-and x y z)))

(defn !
  "Bit-xor shorthand"
  ([x y] (bit-xor x y))
  ([x y & z] (apply bit-xor x y z)))


(defn divides?
  "Check if y divides x"
  [x y]
  (zero? (rem x y)))

(defn divides-not?
  "Check if y doesn't divide x"
  [x y]
  (not (zero? (rem x y))))


(defn xor
  ([data1 data2] (map #(bit-xor %1 %2) data1 data2))
  ([data1 data2 & rst] (map #(apply bit-xor %) (apply map vector (cons data1 (cons data2 rst))))))

(defn hex->int
  [ch]
  (let [c (int ch)]
    (if (< c 97)
      (- c 48)
      (- c 87))))

(defn int->hex
  [c]
  (char (+ c (if (< c 10) 48 87))))


(defn long->bytes
  "Encode long as bytes"
  ([num] (long->bytes num :little))
  ([num endianess]
   (when-not (< num 0xFFFFFFFFFFFFFFFF)
     (throw (Exception. (str "Number exceeds MAX-LONG: " num))))

   (reduce #(conj %1 (& (>> num %2) 0xff))
           (if (= endianess :big) '() [])
           (map #(* % 8) (range 8)))))

(defn bytes->long
  "Decode bytes into long"
  ([bytes] (bytes->long bytes :little))
  ([bytes endianess]
   (when-not (= 8 (count bytes))
     (throw (Exception. "Byte lst should have length 8")))

   (reduce #(+ (<< %1 8) %2) 0 (if (= endianess :big) bytes (reverse bytes)))))


(defn int->bytes
  "Encode int as bytes"
  ([num] (int->bytes num :little))
  ([num endianess]
   (when-not (< num 0xFFFFFFFF)
     (throw (Exception. (str "Number exceeds MAX-INT: " num))))

   (reduce #(conj %1 (& (>> num %2) 0xff))
           (if (= endianess :big) '() [])
           (map #(* % 8) (range 4)))))

(defn bytes->int
  "Decode bytes into int"
  ([bytes] (bytes->int bytes :little))
  ([bytes endianess]
   (when-not (= 4 (count bytes))
     (throw (Exception. "Byte lst should have length 4")))

   (reduce #(+ (<< %1 8) %2) 0 (if (= endianess :big) bytes (reverse bytes)))))


(defn bytes->number
  "Decode byte array into bigint"
  ([num] (bytes->number num :little))
  ([num endianess]

   (reduce #(+ (*' %1 256) %2)
           0
           (if (= endianess :big)
             (reverse num)
             num))))

(defn number->bytes
  "Encode a number as byte array"
  ([num] (number->bytes num :little))
  ([num endianess]

   (loop [x num
          acc (if (= endianess :big) '() [])]
     (if (zero? x)
       acc
       (recur (quot x 256)
              (conj acc (int (rem x 256))))))))


(def MAX-NUM 0xFFFFFFFF)

(defn hexstr->str
  [data]
  (mapv #(char (+ (bit-shift-left (hex->int (first %)) 4) (hex->int (second %))))
        (partition 2 2 0 (seq data))))

(defn down-case
  [c]
  (let [ch (int c)]
    (if (and (< ch 91) (> ch 64))
      (char (+ ch 32))
      c)))

(defn str->lst
  [data]
  (map hex->int (seq data)))

(defn lst->str [data] (clojure.string/join (map int->hex data)))

;; Conversion between byte arrays and ASCII strings
(defn bytes->str [data] (clojure.string/join (map char data)))

(defn str->bytes [data] (map int data))


;; Base 64 to byte array

(defn- base64-to-bits
  [ch]
  (let [c (int ch)]
    (cond
      (and (> c 64) (< c 91)) (- c 65)
      (and (> c 96) (< c 123)) (- c 71)
      (and (> c 47) (< c 58)) (+ c 4)
      (= c 43) 62
      (= c 47) 63
      (= c 61) 0)))

(defn- transform-block
  [block]
  (loop [block (reduce #(+ %2 (bit-shift-left %1 6)) block)
         n 3
         acc []]
    (if (zero? n)
      acc
      (recur (bit-shift-right block 8)
             (dec n)
             (cons (bit-and block 255) acc)))))


(defn- decode-base64-block
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

;; Custom list processors
(defn find-seq
  "Find the index where val starts from in lst"
  [lst val]
  (str/index-of (bytes->str lst) (bytes->str val)))

(defn reduce'
  [f b l & coll]
  (if (empty? l)
    b
    (loop [lst (apply map vector (cons l coll))
           acc b]
      (if (empty? lst)
        acc
        (recur (rest lst) (apply f acc (first lst)))))))

(defn partition'
  "Translates to (partition block-size block-size nil lst)"
  [block-size lst]
  (partition block-size block-size nil lst))

(defn option-map'
  "Maps a function on a seq but removes results that return nil"
  [f l]
  (loop [lsts l
         acc '()]
    (if (empty? lsts)
      (reverse acc)
      (recur (rest lsts) (let [val (f (first lsts))]
                           (if (nil? val)
                             acc
                             (cons val acc)))))))

(defn interleave'
  "Interleave implmentation that works
  for different list sizes"
  [v & vs]
  (let [lsts (cons v vs)
        fst (option-map' #(first %) lsts)]
    (if (empty? fst)
      nil
      (lazy-cat fst
                (apply interleave' (map rest lsts))))))


(defn subseq'
  "Get subsequence starting at idx taking n elements"
  ([lst n] (take n lst))
  ([lst idx n] (take n (drop idx lst))))


(defn gen-reduce
  "Generic reducer. Takes in reducing function and optional end condition."
  [f base taker reducer end? value]
  (loop [val value
         acc base]
    (if (end? val)
      acc
      (recur (reducer val)
             (f acc (taker val))))))

;; Timing tools
(defn get-runtime
  "Time function execution and return result
  and time (in microseconds)"
  [f & args]
  (let [start-time (System/nanoTime)]
    (let [result (apply f args)]
      (let [stop-time (System/nanoTime)]
        [result (quot (- stop-time start-time) 1000)]))))


;; Exception handler
(defn raise
  [& args]
  (throw (util.CryptoException. (apply str args))))
