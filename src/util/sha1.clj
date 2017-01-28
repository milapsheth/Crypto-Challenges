(ns util.sha1
  (:require [util.tools :as u]
            [util.tools :refer [| ! & >> <<]]))


(def HASH-INIT [0x67452301
                0xEFCDAB89
                0x98BADCFE
                0x10325476
                0xC3D2E1F0])

(def MAX-INT32 0xFFFFFFFF)
(def INT32-BIT 0x100000000)
(def MAX-INT64 0xFFFFFFFFFFFFFFFF)



(defn left-rotate
  "Left rotate n by b bits"
  [n b]
  (| (& (<< n b) MAX-INT32)
     (>> n (- 32 b))))


(defn extend-chunk
  "Extends msg into 80 32-bit words"
  [msg]

  (loop [chunk (mapv (fn [word-lst]
                       (reduce #(| (<< %1 8) %2) word-lst))
                     (partition 4 msg))
         i 16]
    (if (< i 80)
      (recur (conj chunk
                   (-> (reduce #(! %1 (chunk (- i %2)))
                               0
                               '(3 8 14 16))
                       (left-rotate 1)))
             (inc i))
      chunk)))


(defn next-h
  "Output next h registers"
  [[a b c d e] chunk i]
  [(-> (cond
         (< i 20) (+ (! (& b c)
                        (& (bit-not b) d))
                     0x5A827999)
         (< i 40) (+ (! b c d)
                     0x6ED9EBA1)
         (< i 60) (+ (! (& b c)
                        (& b d)
                        (& c d))
                     0x8F1BBCDC)
         :else (+ (! b c d)
                  0xCA62C1D6))
       (+ (left-rotate a 5) e (chunk i))
       (& MAX-INT32))
   a
   (left-rotate b 30)
   c
   d])


(defn process-chunk
  "Process one 512-bit chunk of message"
  [h msg]

  (let [chunk (extend-chunk msg)]

    (loop [h' h
           i 0]
      (if (< i 80)

        (recur
         (next-h h' chunk i)
         (inc i))

        (mapv #(& (+ %1 %2) MAX-INT32) h h')))))


(defn process-msg
  "Process conditioned message"
  ([msg] (process-msg msg HASH-INIT))
  ([msg hash-init]
   (reduce process-chunk
           hash-init
           (u/partition' 64 msg))))


(defn pre-process
  "Pre-process the message to make it a multiple of 512 bits"
  ([msg] (pre-process msg (count msg)))
  ([msg len]
   (concat msg
           '(0x80)
           (repeat (mod (- 55 (count msg)) 64) 0)
           (u/long->bytes (* 8 len) :big))))


(defn transform-into-hash
  [h]
  (mapcat #(u/int->bytes % :big) h))


(defn sha1
  "SHA1 hash function"
  ([msg]
   (-> msg
       pre-process
       process-msg
       transform-into-hash))
  ([msg h len] ;; A hash version that allows caller to pass hashing parameters
   (-> msg
       (pre-process len)
       (process-msg h)
       transform-into-hash)))
