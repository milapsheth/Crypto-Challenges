(ns set1.decrypt-vigenere-test
  (:require [clojure.test :refer :all]
            [set1.decrypt-vigenere :refer [decrypt-vigenere]]
            [set1.repeating-xor :as v]
            [clojure.java.io :as io]
            [util.conv :as u]
            [util.random :as random]))


(defn decrypt-cipher
  [ciphertext]
  (clojure.string/join (map char (decrypt-vigenere ciphertext))))


(def ciphertext (with-open [rdr (io/reader (io/file (io/resource "set1/vigenere_ciphertext.txt")))]
                  (u/base64-to-byte' (apply concat (line-seq rdr)))))

(def plaintext (slurp (io/resource "set1/vigenere_plaintext.txt")))


;; Tests

(deftest ^:parallel decrypt-vigenere-test
  (testing "Failed decryption of vigenere cipher"
    (is (= plaintext (decrypt-cipher ciphertext)))))


(deftest ^:parallel encrypt-and-decrypt-corpus
  (testing "Failed decryption of vigenere cipher"
      (let [plaintext (map int (slurp (io/resource "plaintext1.txt")))
            cipher-key (map int (random/random-string (+ 2 (rand-int 39))))]
        (is (= plaintext (decrypt-vigenere (v/encrypt plaintext cipher-key)))))))
