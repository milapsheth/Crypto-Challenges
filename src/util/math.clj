(ns util.math)

(def MAX-INT32 0x7FFFFFFF)

(defn sqr "Square number" [x] (*' x x))

(defn abs "Absolute value of n"
  [n]
  (if (neg? n) (-' n) n))

(defn pow
  "Raise x to power y. Optional modulus can be passed in."
  ([x y]
   (when (< y 0)
     (throw (Exception. "Power cannot be negative")))
   (loop [ans 1
          base x
          e y]
     (if (zero? e)
       ans
       (recur (if (zero? (rem e 2))
                ans
                (*' ans base))
              (sqr base)
              (quot e 2)))))
  ([x y modulus]
   (when (< y 0)
     (throw (Exception. "Power cannot be negative")))
   (loop [ans 1
          base x
          e y]
     (if (zero? e)
       ans
       (recur (if (zero? (rem e 2))
                ans
                (rem (*' ans base) modulus))
              (rem (sqr base) modulus)
              (quot e 2))))))


(defn gcd
  "GCD(a, b)"
  [a b]
  (if (zero? b)
    a
    (recur b (mod a b))))

(defn reduce-power
  "Reduce number by power of given number"
  ([n p] (reduce-power n p 0))
  ([n p acc] (if (zero? (rem n p))
               (recur (quot n p) p (inc acc))
               [acc n])))


(defn legendre
  "Compute legendre symbol"
  [a p]
  (cond
    (even? p) (throw (Exception. "Base has to be odd"))
    (not= (gcd a p) 1) 0
    :else
    (loop [a (mod a p)
           p p
           acc 1]
      (cond
        (= a 1) 1
        (= a 2) (* acc (if (odd? p) 1 -1))
        :else (let [[pow2 a'] (reduce-power a 2) ;; Extract largest power of 2
                    pow2-acc (if (or (even? pow2) (some? #{1 7} (mod p 8)))
                               1 -1)  ;; Compute (2/p)^e
                    p' (mod p a')  ;; New (a/p)
                    acc' (if (zero? (rem (* (rem a' 4)
                                            (rem p 4))
                                         4))
                           1 -1)  ;; Quadratic reciprocity
                    ]
                (if (= a' 1)
                  pow2-acc
                  (recur p' a' (* acc acc' pow2-acc)))
                )))))

;; Operations on Integers modulo n
(defn sqrt
  "Perform square root under mod n"
  [x n]
  (throw (Exception. "Not implemented")))
