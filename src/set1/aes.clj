(ns set1.aes
  (:require [set1.aes-constants :as constants]
   [util.conv :as u]))


;; AES algorithm description
;; Translated to clojure from http://anh.cs.luc.edu/331/code/aes.py

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

;; Key schedule core
(defn key-schedule-core
  [word iter]
  (->> (conj (subvec word 1) (word 0))
      (map constants/s-box)
      ((fn [w] (cons (bit-xor (first w) (constants/rcon iter)) (rest w))))))

;; Expand key
(defn expand-key
  "Key expansion function
  Expands an 128,192,256 key into an 176,208,240 bytes key"
  [key initial-size expanded-key-size]
  (loop [current-size initial-size
         rcon-iter 1
         expanded-key (vec key)]
    (if (>= current-size expanded-key-size)
      expanded-key
      (recur (+ current-size 4)
             (if (zero? (rem current-size initial-size))
               (inc rcon-iter)
               rcon-iter)
             (->> (subvec expanded-key (- current-size 4) current-size)
                  ((fn [k] (if (zero? (rem current-size initial-size))
                             (key-schedule-core k rcon-iter)
                             k)))
                  ((fn [k] (if (and (= initial-size 32) (= (rem current-size initial-size) 16))
                            (map constants/s-box k) ;; For 256-bit keys perform another s-box transform
                            k)))
                  (map #(bit-xor %1 %2) (subvec expanded-key (- current-size initial-size) (- current-size initial-size -4))) ;; XOR generated 4 bytes with previous 4 bytes to expand key
                  (into expanded-key))))))


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
    (map-indexed #(concat (drop (idx %1) %2) (take (idx %1) %2)) state)))

;; Mix columns step

(defn galois-mult
  "Multiplication of 8-bit ints in a galois field"
  [x y]
  (loop [i 8 ans 0
         a x
         b y]
    (if (zero? i)
      ans
      (let [high-bit-not-set (zero? (bit-and a 0x80))]
        (recur (dec i)
               (if-not (zero? (bit-and b 1)) (bit-xor ans a) ans)
               (let [new-a (bit-and (bit-shift-left a 1) 0xFF)]
                 (if-not high-bit-not-set (bit-xor new-a 0x1b) new-a))
               (bit-shift-right b 1))))))

(defn mix-column
  "Mix columns"
  [column inv?]
  (def mult (if-not inv? [2, 1, 1, 3] [14, 9, 13, 11]))
  (map (fn [order] (u/reduce' #(bit-xor %1 (galois-mult (column %2) %3))
                              0 order mult))
       [[0 3 2 1], [1 0 3 2], [2 1 0 3], [3 2 1 0]]))

(defn mix-columns
  [state inv?]
  ;; (println state)
  (apply map vector
         (map #(mix-column % inv?)
              (apply map vector state))))

;; Add round key step
(defn add-round-key
  "Add round key step"
  [state round-key]
  (map #(map (fn [p k] (bit-xor p k)) %1 %2)
       state (apply map vector (partition 4 4 round-key))))

;; Create round key
(defn create-round-key
  "Create a round key.
  Creates a round key from the given expanded key and the
  position within the expanded key.
  "
  [expanded-key round-key-pointer]
  (subvec expanded-key round-key-pointer (+ round-key-pointer 16))
  #_ (apply mapv vector (partition 4 4 (subvec expanded-key round-key-pointer (+ round-key-pointer 16)))))

;; Encryption round
(defn aes-encrypt-round
  "One round of AES encryption"
  [state round-key]
  ;; (println "Round")
  ;; (println state)
  (-> state
      ;;((fn [s] (println "In round:") (println s) s))
      (sub-bytes false)
      (shift-rows false)
      (mix-columns false)
      (add-round-key round-key)))


;; Decryption round
(defn aes-decrypt-round
  "One round of AES encryption"
  [state round-key]
  (-> state
      (shift-rows true)
      (sub-bytes true)
      (add-round-key round-key)
      (mix-columns true)))


(defn aes-encrypt
  "AES encryption of one block"
  [block cipher-key key-size]
  (let [rounds (constants/num-rounds key-size)
        expanded-key (expand-key cipher-key key-size (constants/expanded-key-size key-size))]
    (-> (apply mapv vector (partition 4 4 block))
        (add-round-key (create-round-key expanded-key 0))
        ((fn [s] (println s) s))
        ((fn [s] (loop [i 1 new-s s] (if (< i rounds)
                                       (recur (inc i) (aes-encrypt-round new-s (create-round-key expanded-key (* 16 i))))
                                       new-s))))
        (sub-bytes false)
        (shift-rows false)
        (add-round-key (create-round-key expanded-key (* 16 rounds))))))

;; (aes-encrypt (range 16) (expand-key (range 32) 32 240) 14)

(defn aes-decrypt
  "AES decryption of one block"
  [block cipher-key key-size]
  (let [rounds (constants/num-rounds key-size)
        expanded-key (expand-key cipher-key key-size rounds)]
    (-> (apply mapv vector (partition 4 4 block))
        (add-round-key (create-round-key (vec expanded-key) (* 16 rounds)))
        ((fn [s] (loop [i (dec rounds) new-s s] (if (< i rounds)
                                                  (recur (dec i) (aes-decrypt-round new-s (create-round-key expanded-key (* 16 i))))
                                                  new-s))))
        (shift-rows true)
        (sub-bytes true)
        (add-round-key (create-round-key expanded-key 0)))))
  

;; Encrypt AES block
