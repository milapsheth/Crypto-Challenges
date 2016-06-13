(ns set1.aes
  (:require [set1.aes-constants :as constants]
   [util.conv :as u]))


;; AES algorithm description

;; Key size: num_rounds => 128: 10, 192: 12, 256: 14

;; State: 16 byte block represented as matrix
;; [a0  a1  a2  a3
;;  a4  a5  a6  a7
;;  a8  a9  a10 a11
;;  a12 a13 a14 a15]


;; 1) Key expansion

;; 2) Initial round: add-round-key

;; 3) Rounds
;;   * sub-bytes - a non-linear substitution step where each byte
;; 		   is replaced with another according to a lookup table
;;   * shift-rows - a transposition step where the last three rows of
;; 	 	    the state are shifted cyclically a certain number
;; 		    of steps
;;   * mix-columns - a mixing operation which operates on the columns of
;; 		     the state, combining the four bytes in each column
;;   * add-round-key - each byte of the state is combined with a block
;;                     of the round key using bitwise xor

;; 4) Final round
;;   * sub-bytes
;;   * shift-rows
;;   * add-round-key


;; AES functions

;; Sub bytes step
(defn sub-bytes  "Replace a byte with the corresponding
  value at index in the Rijndael S-box"
  [state inv?]
  (if-not inv?
    (map #(map constants/s-box %) state)
    (map #(map constants/inverse-s-box %) state)))

;; Shift rows step
(defn shift-rows
  "Rotate state rows"
  [state inv?]
  (let [idx (if-not inv? (fn [i] i) (fn [i] (- 4 i)))]
    (map-indexed #(concat (drop (idx %1) %2) (take (idx %1) %2))
                 (partition 4 4 state))))

;; Mix columns step

(defn galois-mult
  "Multiplication of 8-bit ints in a galois field"
  [x y]
  (loop [i 8 ans 0
         a x
         b y]
    (if (zero? i)
      ans
      (let [high-bit-set (bit-and a 0x80)]
        (recur (dec i)
               (if-not (zero? (bit-and b 1)) (bit-xor ans a) ans)
               (let [new-a (bit-and (bit-shift-left a 1) 0xFF)]
                 (if-not (zero? high-bit-set) (bit-and new-a 0x1b) new-a))
               (bit-shift-right b 1))))))

(defn mix-column
  "Mix columns"
  [column inv?]
  (def mult (if-not inv? [2, 1, 1, 3] [14, 9, 13, 11]))
  (map (fn [order] (u/reduce' #(bit-xor %1 (column %2) %3)
                              0 order mult))
       [[0 3 2 1], [1 0 3 2], [2 1 0 3], [3 2 1 0]]))

(defn mix-columns
  [state inv?]
  (apply interleave (map #(mix-column % inv?) (apply map vector state))))

;; Add round key step
(defn add-round-key
  "Add round key step"
  [state cipher-key]
  (map #(map (fn [arg] (bit-xor (first arg) (second arg))) %) state cipher-key))

