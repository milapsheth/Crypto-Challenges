(ns set1.aes
  (:require [set1.aes-constants :as c]
            [set1.padding :refer [pad-plaintext unpad-plaintext]]
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
      (map c/s-box)
      ((fn [w] (cons (bit-xor (first w) (c/rcon iter)) (rest w))))))

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
                            (map c/s-box k) ;; For 256-bit keys perform another s-box transform
                            k)))
                  (map #(bit-xor %1 %2) (subvec expanded-key (- current-size initial-size) (- current-size initial-size -4))) ;; XOR generated 4 bytes with previous 4 bytes to expand key
                  (into expanded-key))))))


;; Sub bytes step
(defn sub-bytes
  "Replace a byte with the corresponding
  value at index in the Rijndael S-box"
  [state inv?]
  (if-not inv?
    (map #(map c/s-box %) state)
    (map #(map c/inverse-s-box %) state)))

;; Shift rows step
(defn shift-rows
  "Rotate state rows.
  ith row rotates i times."
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
  "Rijndael mix columns step."
  [column inv?]
  (def mult (if-not inv? [2, 1, 1, 3] [14, 9, 13, 11]))
  (map (fn [order] (u/reduce' #(bit-xor %1 (galois-mult (column %2) %3))
                              0 order mult))
       [[0 3 2 1], [1 0 3 2], [2 1 0 3], [3 2 1 0]]))

(defn mix-columns
  [state inv?]
  (apply map vector
         (map #(mix-column % inv?)
              (apply map vector state))))

;; Add round key step
(defn add-round-key
  "Add round key step"
  [state round-key]
  (map #(map (fn [p k] (bit-xor p k)) %1 %2)
       state round-key))

;; Create round key
(defn create-round-key
  "Create a round key.
  Creates a round key from the given expanded key and the
  position within the expanded key.
  "
  [expanded-key round-key-pointer]
  (apply map vector
         (partition 4 4 (subvec expanded-key round-key-pointer (+ round-key-pointer 16)))))


;; Encryption round
(defn aes-encrypt-round
  "One round of AES encryption"
  [state round-key]
  (-> state
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
  (let [rounds (c/num-rounds key-size)
        expanded-key (expand-key cipher-key key-size (c/expanded-key-size key-size))]
    (-> (apply mapv vector (partition 4 4 block))
        (add-round-key (create-round-key expanded-key 0))
        ((fn [s] (loop [i 1 new-s s] (if (< i rounds)
                                       (recur (inc i) (aes-encrypt-round new-s (create-round-key expanded-key (* 16 i))))
                                       new-s))))
        (sub-bytes false)
        (shift-rows false)
        (add-round-key (create-round-key expanded-key (* 16 rounds)))
        ((fn [state] (apply mapcat vector state))))))


(defn aes-decrypt
  "AES decryption of one block"
  [block cipher-key key-size]
  (let [rounds (c/num-rounds key-size)
        expanded-key (expand-key cipher-key key-size (c/expanded-key-size key-size))]
    (-> (apply mapv vector (partition 4 4 block))
        (add-round-key (create-round-key expanded-key (* 16 rounds)))
        ((fn [s] (loop [i (dec rounds) new-s s] (if (zero? i)
                                                  new-s
                                                  (recur (dec i) (aes-decrypt-round new-s (create-round-key expanded-key (* 16 i))))))))
        (shift-rows true)
        (sub-bytes true)
        (add-round-key (create-round-key expanded-key 0))
        ((fn [state] (apply mapcat vector state))))))
  

;; Modes of Encryption

;; ECB mode (this is the shitty one)
(defn encrypt-ecb
  "Encrypt input with AES.
  Padding is done for input if required"
  [plaintext cipher-key]
  (def key-size (count cipher-key))

  (def padded-plaintext (pad-plaintext plaintext c/BLOCK-SIZE :pkcs7))
  
  (mapcat #(aes-encrypt % cipher-key key-size)
          (partition c/BLOCK-SIZE c/BLOCK-SIZE 0 padded-plaintext)))


(defn decrypt-ecb
  "Decrypt input with AES.
  Padding is done for input if required"
  [ciphertext cipher-key]
  (def key-size (count cipher-key))

  (-> (mapcat #(aes-decrypt % cipher-key key-size)
              (partition c/BLOCK-SIZE c/BLOCK-SIZE (repeat 0) ciphertext))
      (unpad-plaintext c/BLOCK-SIZE :pkcs7)))


;; CBC mode of operation
(defn encrypt-cbc
  "Encrypt plaintext using AES under CBC mode"
  [plaintext cipher-key iv]
  (let [key-size (count cipher-key)]
    
    (loop [padded-text (partition c/BLOCK-SIZE c/BLOCK-SIZE
                                  (pad-plaintext plaintext c/BLOCK-SIZE :pkcs7))
           encrypted-block iv
           ciphertext '()]
      
      (if (empty? padded-text)
        (apply concat (reverse ciphertext))

        (let [encrypted-block (aes-encrypt (u/xor (first padded-text) encrypted-block) cipher-key key-size)]
          (recur (rest padded-text)
                 encrypted-block
                 (cons encrypted-block ciphertext)))))))


(defn decrypt-cbc
  "Decrypt ciphertext using AES under CBC mode"
  [ciphertext cipher-key iv]
  (when-not (= c/BLOCK-SIZE (count iv))
    (Exception. "IV should be 16 bits"))
  
  (let [key-size (count cipher-key)]
    (loop [ciphertext (partition c/BLOCK-SIZE c/BLOCK-SIZE ciphertext)
           decrypted-block iv
           plaintext '()]
      
      (if (empty? ciphertext)
        (unpad-plaintext (apply concat (reverse plaintext))
                         c/BLOCK-SIZE :pkcs7)

        (let [decrypted-block (u/xor (aes-decrypt (first ciphertext) cipher-key key-size)
                                   decrypted-block)]
          (recur (rest ciphertext)
                 (first ciphertext)
                 (cons decrypted-block plaintext)))))))


;; Generic AES encryption/decryption functions

(defn encrypt
  "Encrypts plaintext using AES under given mode"
  [plaintext cipher-key mode & args]

  (when-not (and (c/possible-key-size? (count cipher-key))
                 (zero? (rem (count plaintext) c/BLOCK-SIZE)))
    (Exception. "Invalid param length"))
  
  (let [functions {:ecb encrypt-ecb, :cbc encrypt-cbc}]
    (apply (functions mode)
           plaintext cipher-key args)))


(defn decrypt
  "Decrypts ciphertext using AES under given mode"
  [ciphertext cipher-key mode & args]

  (when-not (and (c/possible-key-size? (count cipher-key))
                 (zero? (rem (count ciphertext) c/BLOCK-SIZE)))
    (Exception. "Invalid param length"))
  
  (let [functions {:ecb decrypt-ecb, :cbc decrypt-cbc}]
    (apply (functions mode)
           ciphertext cipher-key args)))
