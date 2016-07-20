(ns set2.break-ecb-simple-test
  (:require [clojure.test :refer :all]
            [set2.break-ecb-simple :as sut]
            [util
             [aes :as aes]
             [tools :as u]]))

(def random-cipher-key (repeatedly 16 #(rand-int 256)))

(defn oracle-encrypt
  [plaintext unknown-string]
  (aes/encrypt (concat plaintext unknown-string) random-cipher-key :ecb))


(deftest break-ecb-basic-test
  (testing "Failed to break ECB mode on sample string"
    (let [unknown-string (map int "UNKNOWN STRING")]
      (is (= unknown-string
             (sut/break-ecb #(oracle-encrypt % unknown-string)))))))

;; Actual challenge test
;; Not run as part of the suite as it takes a bit of time to run

(def unknown-string (u/base64-to-byte' "Um9sbGluJyBpbiBteSA1LjAKV2l0aCBteSByYWctdG9wIGRvd24gc28gbXkgaGFpciBjYW4gYmxvdwpUaGUgZ2lybGllcyBvbiBzdGFuZGJ5IHdhdmluZyBqdXN0IHRvIHNheSBoaQpEaWQgeW91IHN0b3A/IE5vLCBJIGp1c3QgZHJvdmUgYnkK"))


#_
(deftest break-ecb-simple-test
  (testing "Failed to break ECB mode(simple)"
    (is (= unknown-string
           (sut/break-ecb #(oracle-encrypt % unknown-string))))))
