(ns set4.ctr-bitflip
  (:require [util.tools :as u]))

(defn get-prefix-len
  "Obtain the prefix length of ciphertext"
  [oracle]
  ;; To account for the case when the first byte of suffix is equal to
  ;; that of the chosen plaintext, we choose the best of 3 tries (atmost 1 try will differ)
  (-> (loop [tries 3
             acc '()]
        (if (zero? tries)
          acc
          (let [cipher1 (vec (oracle (repeat 10 (+ 65 tries))))
                cipher2 (vec (oracle (repeat 20 (+ 65 tries))))
                len (count cipher1)]
            (recur (dec tries)
                   (cons (loop [i 0]
                           (cond
                             (>= i len) (throw (Exception. "Couldn't find the prefix"))
                             (not= (cipher1 i) (cipher2 i)) (- i 10) ;; Found the first
                             :else (recur (inc i))))
                         acc)))))
      ((fn [[x y z]]
         (cond
           (= x y z) x
           (or (= x y) (= x z)) x
           (= y z) y
           :else (throw (Exception. (str "Ambiguous prefix lengths detected " [x y z]))))))))


(defn ctr-bitflip-attack
  "CTR Bitflip modification"
  [oracle malicious-text]

  (let [prefix-len (get-prefix-len oracle)
        text-len (count malicious-text)
        known-text (repeat text-len (int \A))
        ciphertext (oracle known-text)]

    (-> ciphertext
        (u/subseq' prefix-len text-len)
        (u/xor known-text malicious-text)
        (#(concat (take prefix-len ciphertext)
                  %
                  (drop (+ prefix-len text-len) ciphertext)))))
  )
