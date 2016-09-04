(ns util.hmac)


(defn hmac
  "HMAC hashing fucntion"
  [hash-func blocksize key msg]
  (let [new-key (cond (< blocksize (count key)) (hash-func key)
                      (> blocksize (count key)) (concat key (repeat (- blocksize (count key)) 0))
                      :else key)]
    (->> (concat (map #(bit-xor % 0x36) new-key) msg)
         hash-func
         (concat (map #(bit-xor % 0x5c) new-key))
         hash-func)))
