(ns set3.break-mt
  (:require [util.tools :as u]
            [util.tools :refer [& ! << >>]]
            [util.mersenne-twister :as mt]))



(def MAX-INT32 0xFFFFFFFF)
(def INT32-BIT 0x80000000)
(def MAX-INT31 0x7FFFFFFF)

(def TEMPER-MASK1 2636928640)
(def TEMPER-MASK2 4022730752)

(def BIT-MASK1 1812433253)
(def BIT-MASK2 2567483615)

;; MT state
(def STATE-LEN 624)

(def STATE (atom [(vec (repeat 624 0)) ;; state
                  0 ;; index
                  0])) ;; last-random-number generated


(defn- twist-i
  "Twist ith number of state"
  [MT i]
  (let [y (+ (& (MT i) INT32-BIT)
             (& (MT (rem (inc i) STATE-LEN))
                  MAX-INT31))]
    (cond-> (! (MT (rem (+ i 397) STATE-LEN))
                 (>> y 1))
      (u/divides-not? y 2) (! BIT-MASK2))))

(defn twist
  "Generate next mersenne twister state"
  [MT]
  (mapv #(twist-i MT %) (range STATE-LEN)))


(defn predict-next
  "Output next predicted random number"
  []
  (get (swap! STATE (fn [[MT index _]]
                      (let [MT (if (zero? index) (twist MT) MT)]
                        [MT
                         (rem (inc index) STATE-LEN)
                         (-> (MT index)
                             (#(! % (>> % 11)))
                             (#(! % (& (<< % 7) TEMPER-MASK1)))
                             (#(! % (& (<< % 11) TEMPER-MASK2)))
                             (#(! % (>> % 18))))]
                        )))
       2)) ;; Update state and output the last generated random number


(defn rand-num
  "Return a pseudo-random number in the given range a <= x < b"
  ([b] (mod (predict-next) b))
  ([a b] (+ a (mod (predict-next) (- b a)))))


(defn left-shift-untemper
  [num shift mask]
  (loop [y (& num (dec (<< 1 shift)))
         s (* 2 shift)]
    (if (>= s 32)
      (! (& (<< y shift) mask)
         (& num (dec (<< 1 32))))
      (recur (! (& (<< y shift) mask)
                (& num (dec (<< 1 s))))
             (+ s shift)))))

(defn right-shift-untemper
  [num shift]
  (loop [y num
         s shift]
    (if (>= s 16)
      (! (>> y shift) num)
      (recur (! (>> y shift) num)
             (+ s shift)))))


(defn untemper
  "Untemper the pseudo random number provided"
  [num]
  (-> num
      (right-shift-untemper 18)
      (left-shift-untemper 11 TEMPER-MASK2)
      (left-shift-untemper 7 TEMPER-MASK1)
      (right-shift-untemper 11)))


(defn clone-same-state
  "Clone current state of the generator 
  from random numbers extracted for one twist"
  [random-nums]
  (when-not (= (count random-nums) STATE-LEN)
    (throw (Exception. "Required 624 numbers to copy Mersenne twister state")))
  (reset! STATE [(mapv untemper random-nums) 0 0])
  nil)


(defn check-for-right-state
  [state index]
  (let [vec (subvec state index (+ index STATE-LEN))]
    (loop [i index]
      (if (>= i STATE-LEN)
        true
        (and (= (twist-i vec (- i index))
                (state (+ i STATE-LEN)))
             (recur (inc i)))))))


(defn clone-state
  "Clone the current state of the twister from sequence of inputs
  (at least twice the number of state elements provided)"
  [random-nums]
  (when-not (>= (count random-nums) (* 2 STATE-LEN))
    (throw (Exception. "Required at least 2 * 624 numbers")))

  (let [rnums (mapv untemper (drop (- (count random-nums) (* 2 STATE-LEN)) random-nums))
        state (subvec rnums 0 STATE-LEN)
        remaining (subvec rnums STATE-LEN (* 2 STATE-LEN))]

    (loop [i 0]
      (if (>= i STATE-LEN)
        (throw (Exception. "Couldn't guess the state. Maybe the numbers are out of order"))

        (if (check-for-right-state rnums i)
          (do
            (reset! STATE
                    [(twist (subvec rnums i (+ i STATE-LEN)))
                     (rem (- STATE-LEN i) STATE-LEN)
                     0])
            nil)
          (recur (inc i)))))))
