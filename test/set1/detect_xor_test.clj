(ns set1.detect-xor-test
  (:require [clojure.test :refer :all]
            [clojure.java.io :as io]
            [set1.detect-xor :refer :all]))


(def plaintext "Now that the party is jumping\n")

(def ciphertext (clojure.string/split-lines (slurp (io/resource "set1/caesar_ciphertext.txt"))))

;; Detect the encrypted message
(deftest detect-xor-test
  (testing "Detected wrong message"
    (is (= (detect-encrypted ciphertext) plaintext))))
