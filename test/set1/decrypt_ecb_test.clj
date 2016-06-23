(ns set1.decrypt-ecb-test
  (:require [set1.aes :as aes]
            [clojure.java.io :as io]
            [clojure.test :refer :all]
            [util.conv :as u]
            [clojure.string :as str]))


(def cipher-key (map int "YELLOW SUBMARINE"))

(def ciphertext (with-open [rdr (io/reader (io/file (io/resource "set1/decrypt_aes.txt")))]
                  (u/base64-to-byte' (apply concat (line-seq rdr)))))

(def plaintext (slurp (io/resource "set1/decrypt_aes_plaintext.txt")))


(defn decrypt-ecb
  [cipher]
  (u/bytes-to-str (aes/decrypt cipher cipher-key :ecb)))


(deftest decrypt-ecb-test
  (testing "Couldn't decrypt ciphertext using AES(ECB)"
    (is (= (decrypt-ecb ciphertext) plaintext))))

(deftest aes-test
  (def plaintext "Check if my AES implementation encryption works")
  (def cipher-key (map int "YELLOW SUBMARINE"))
  
  (testing "AES encryption/decryption doesn't work"
    (is (= plaintext
           (decrypt-ecb (aes/encrypt (map int plaintext) cipher-key :ecb))))))
