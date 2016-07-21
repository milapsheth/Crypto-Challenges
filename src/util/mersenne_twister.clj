(ns util.mersenne-twister
  (:require [util.tools :as u]))


(def MAX-INT32 0xFFFFFFFF)
(def INT32-BIT 0x80000000)
(def MAX-INT31 0x7FFFFFFF)

(def TEMPER-MASK1 2636928640)
(def TEMPER-MASK2 4022730752)

(def BIT-MASK1 1812433253)
(def BIT-MASK2 2567483615)

;; MT state
(def STATE (atom [(vec (repeat 624 0)) ;; state
                  0 ;; index
                  0])) ;; last-random-number generated

(def STATE-LEN 624)


(defn initialize-generator
  "Initialize random number generator with provided seed."
  [seed]
  (reset! STATE
          (loop [i 1
                 MT [(u/& MAX-INT32 seed)]]
            (if (>= i STATE-LEN)
              [MT 0 0]
              (recur (inc i)
                     (conj MT
                           (u/& MAX-INT32
                                (u/! (* BIT-MASK1 (peek MT))
                                     (+ i (u/>> (peek MT)
                                                30)))))))))
  nil)


(defn twist
  "Generate next mersenne state"
  [MT]
  (mapv (fn [i]
          (let [y (+ (u/& (MT i) INT32-BIT)
                     (u/& (MT (rem (inc i) STATE-LEN))
                          MAX-INT31))]
            (cond-> (u/! (MT (rem (+ i 397) STATE-LEN))
                         (u/>> y 1))
              (u/divides-not? y 2) (u/! BIT-MASK2))))
        
        (range STATE-LEN)))


(defn extract-number
  "Output random number"
  []
  (get (swap! STATE (fn [[MT index _]]
                      (let [MT (if (zero? index) (twist MT) MT)]
                        [MT
                         (rem (inc index) STATE-LEN)
                         (-> (MT index)
                             (#(u/! % (u/>> % 11)))
                             (#(u/! % (u/& (u/<< % 7) TEMPER-MASK1)))
                             (#(u/! % (u/& (u/<< % 11) TEMPER-MASK2)))
                             (#(u/! % (u/>> % 18))))]
                        )))
       2)) ;; Update state and output the last generated random number


(defn rand-num
  "Return a pseudo-random number in the given range a <= x < b"
  ([b] (mod (extract-number) b))
  ([a b] (+ a (mod (extract-number) (- b a)))))

;; Initialize PRNG
;; (initialize-generator 0)
