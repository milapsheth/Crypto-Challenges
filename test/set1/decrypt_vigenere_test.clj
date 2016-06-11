(ns set1.decrypt-vigenere-test
  (:require [clojure.test :refer :all]
            [set1.decrypt-vigenere :refer [decrypt-vigenere]]
            [clojure.java.io :as io]
            [clojure.string :as s]
            [util.conv :as u]))


(defn decrypt-cipher
  [ciphertext]
  (s/join (map char (decrypt-vigenere ciphertext))))

(def ciphertext (with-open [rdr (io/reader (io/file (io/resource "set1/vigenere_ciphertext.txt")))]
                  (u/base64-to-byte' (apply concat (line-seq rdr)))))

(def plaintext (slurp (io/resource "set1/vigenere_plaintext.txt")))

(deftest decrypt-vigenere-test
  (testing "Couldn't decrypt correctly"
    (is (= plaintext (decrypt-cipher ciphertext)))))
