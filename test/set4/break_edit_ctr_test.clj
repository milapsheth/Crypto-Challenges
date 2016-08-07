(ns set4.break-edit-ctr-test
  (:require [clojure.test :refer :all]
            [set4.break-edit-ctr :as sut]
            [util.random :as rand]
            [util.aes :as aes]
            [util.tools :as u]))


(def cipher-key (rand/byte-lst 16))

(def nonce (rand/byte-lst 8))

(defn encrypt
  [plaintext]
  (aes/encrypt plaintext cipher-key :ctr nonce))

(defn edit
  [ciphertext offset newtext]
  (-> ciphertext
      (aes/decrypt cipher-key :ctr nonce)
      (#(take offset %))
      (concat newtext)
      (aes/encrypt cipher-key :ctr nonce)))


(def plaintext (u/str->bytes "Awesome man!"))

(def ciphertext (encrypt plaintext))

(deftest break-edit-ctr-test
  (testing "Failed to break random access read/write CTR"
    (is (= plaintext
           (sut/break-ctr ciphertext edit)))))
