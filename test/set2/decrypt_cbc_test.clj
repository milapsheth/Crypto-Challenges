(ns set2.decrypt-cbc-test
  (:require [clojure.java.io :as io]
            [clojure.test :refer :all]
            [util
             [aes :as aes]
             [random :as r]
             [tools :as u]]))

(def ciphertext (with-open [rdr (io/reader (io/file (io/resource "set2/decrypt_cbc_ciphertext.txt")))]
                  (u/base64-to-byte' (apply concat (line-seq rdr)))))

(def plaintext (slurp (io/resource "set2/decrypt_cbc_plaintext.txt")))

(def cipher-key (map int "YELLOW SUBMARINE"))

(def iv (repeat 16 0))

(deftest decrypt-cbc-test
  (testing "Failed challenge: decryption of CBC ciphertext"
    (is (= plaintext
           (u/bytes->str (aes/decrypt ciphertext cipher-key :cbc iv))))))

(deftest aes-cbc-test 
  (testing "Testing CBC mode encryption"

    (let [plaintext "Check if my AES implementation encryption works"
          cipher-key (map int "AES is awesome!!")
          iv (repeatedly 16 #(rand-int 256))]
      (is (= plaintext
             (-> (aes/encrypt (map int plaintext) cipher-key :cbc iv)
                 (aes/decrypt cipher-key :cbc iv)
                 (u/bytes->str)))))))

(deftest aes-cbc-test2
  (testing "Testing CBC mode encryption"

    (let [plaintext (repeatedly (+ 50 (rand-int 50)) #(rand-int 256))
          cipher-key (repeatedly 32 #(rand-int 256))
          iv (repeatedly 16 #(rand-int 256))]
      (is (= plaintext
             (-> (aes/encrypt (map int plaintext) cipher-key :cbc iv)
                 (aes/decrypt cipher-key :cbc iv)))))))
