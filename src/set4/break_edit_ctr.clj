(ns set4.break-edit-ctr
  (:require [util.tools :as u]))


(defn break-ctr
  "Break CTR mode using random access read/write"
  [ciphertext edit-oracle]
  (edit-oracle ciphertext 0 ciphertext))
