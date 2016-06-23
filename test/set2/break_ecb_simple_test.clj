(ns set2.break-ecb-simple-test
  (:require [set2.break-ecb-simple :as sut]
            [clojure.test :refer :all]
            [set1.aes :as aes]
            [util.conv :as u]))


(def random-cipher-key (repeatedly 16 #(rand-int 256)))

(defn oracle-encrypt
  [plaintext unknown-string]
  (aes/encrypt (concat plaintext unknown-string) random-cipher-key :ecb))


(deftest break-ecb-basic-test
  (testing "Failed to break ECB mode on sample string"
    (let [unknown-string (map int "UNKNOWN STRING")]
      (is (= unknown-string
             (sut/break-ecb #(oracle-encrypt % unknown-string)))))))

(def unknown-string (u/base64-to-byte' "Um9sbGluJyBpbiBteSA1LjAKV2l0aCBteSByYWctdG9wIGRvd24gc28gbXkgaGFpciBjYW4gYmxvdwpUaGUgZ2lybGllcyBvbiBzdGFuZGJ5IHdhdmluZyBqdXN0IHRvIHNheSBoaQpEaWQgeW91IHN0b3A/IE5vLCBJIGp1c3QgZHJvdmUgYnkK"))

(deftest break-ecb-simple-test
  (testing "Failed to break ECB mode(simple)"
    (is (= unknown-string
           (sut/break-ecb #(oracle-encrypt % unknown-string))))))
