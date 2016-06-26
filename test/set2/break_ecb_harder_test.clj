(ns set2.break-ecb-harder-test
  (:require [set2.break-ecb-harder :as sut]
            [clojure.test :refer :all]
            [set1.aes :as aes]
            [util.conv :as u]
            [util.random :as rand]))


(def random-cipher-key (rand/byte-lst 16))

(def random-prefix (rand/byte-lst (rand-int 32)))


(defn oracle-encrypt
  [plaintext unknown-string]
  (aes/encrypt (concat random-prefix plaintext unknown-string) random-cipher-key :ecb))
;; (def unknown-string (map int "UNKNOWN STRING"))

(deftest ^:parallel break-ecb-basic-test
  (testing "Failed to break ECB mode on sample string"
    (let [unknown-string (map int "MY UNKNOWN STRING")]
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
