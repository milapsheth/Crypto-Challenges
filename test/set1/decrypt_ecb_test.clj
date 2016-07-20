(ns set1.decrypt-ecb-test
  (:require [clojure
             [string :as str]
             [test :refer :all]]
            [clojure.java.io :as io]
            [util
             [aes :as aes]
             [tools :as u]]))

(def cipher-key (map int "YELLOW SUBMARINE"))

(def ciphertext (with-open [rdr (io/reader (io/file (io/resource "set1/decrypt_aes.txt")))]
                  (u/base64-to-byte' (apply concat (line-seq rdr)))))

(def plaintext (slurp (io/resource "set1/decrypt_aes_plaintext.txt")))


(defn decrypt-ecb
  [cipher]
  (u/bytes->str (aes/decrypt cipher cipher-key :ecb)))


(deftest decrypt-ecb-test
  (testing "Couldn't decrypt ciphertext using AES(ECB)"
    (is (= (decrypt-ecb ciphertext) plaintext))))

(deftest aes-test
  (def plaintext "Check if my AES implementation encryption works")
  (def cipher-key (map int "YELLOW SUBMARINE"))
  
  (testing "AES encryption/decryption doesn't work"
    (is (= plaintext
           (decrypt-ecb (aes/encrypt (map int plaintext) cipher-key :ecb))))))
