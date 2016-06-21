(ns set2.decrypt-cbc-test
  (:require [clojure.test :refer :all]
            [set1.aes :as aes]
            [clojure.java.io :as io]
            [util.conv :as u]
            [util.random :as r]))


(def ciphertext (with-open [rdr (io/reader (io/file (io/resource "set2/decrypt_cbc_ciphertext.txt")))]
                  (u/base64-to-byte' (apply concat (line-seq rdr)))))

(def plaintext (slurp (io/resource "set2/decrypt_cbc_plaintext.txt")))

(def cipher-key (map int "YELLOW SUBMARINE"))

(def iv (repeat 16 0))

(deftest decrypt-cbc-test
  (testing "Failed challenge: decryption of CBC ciphertext"
    (is (= plaintext
           (u/bytes-to-str (aes/decrypt ciphertext cipher-key :cbc iv))))))

(deftest aes-cbc-test 
  (testing "Testing CBC mode encryption"

    (let [plaintext "Check if my AES implementation encryption works"
          cipher-key (map int "AES is awesome!!")
          iv (repeatedly 16 #(rand-int 256))]
      (is (= plaintext
             (-> (aes/encrypt (map int plaintext) cipher-key :cbc iv)
                 (aes/decrypt cipher-key :cbc iv)
                 (u/bytes-to-str)))))))

(deftest aes-cbc-test2
  (testing "Testing CBC mode encryption"

    (let [plaintext (repeatedly (+ 50 (rand-int 50)) #(rand-int 256))
          cipher-key (repeatedly 32 #(rand-int 256))
          iv (repeatedly 16 #(rand-int 256))]
      (is (= plaintext
             (-> (aes/encrypt (map int plaintext) cipher-key :cbc iv)
                 (aes/decrypt cipher-key :cbc iv)))))))
