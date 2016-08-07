(ns util.math)

(def MAX-INT32 0x7FFFFFFF)

(defn sqr "Square number" [x] (*' x x))

(defn pow
  "Raise x to power y. Optional modulus can be passed in."
  ([x y]
   (when (< y 0)
     (throw (Exception. "Power cannot be negative")))
   (loop [ans 1N
          base (bigint x)
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
          base (bigint x) #_(if (or (> x MAX-INT32)
                       (> modulus MAX-INT32))
                 (bigint x)
                 x)
          e y]
     (if (zero? e)
       ans
       (recur (if (zero? (rem e 2))
                ans
                (mod (*' ans base) modulus))
              (mod (sqr base) modulus)
              (quot e 2))))))
