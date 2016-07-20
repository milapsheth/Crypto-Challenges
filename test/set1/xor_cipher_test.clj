(ns set1.xor-cipher-test
  (:require [clojure.java.io :as io]
            [clojure.test :refer :all]
            [set1.xor-cipher :refer [decrypt-caesar encrypt-caesar]]
            [util.tools :as u]))

(defn decrypt
  [data]
  (clojure.string/join (map char (decrypt-caesar (map int (u/hexstr->str data))))))

(def plaintext "Cooking MC's like a pound of bacon")

(def ciphertext (decrypt "1b37373331363f78151b7f2b783431333d78397828372d363c78373e783a393b3736"))

;; Tests

(deftest xor-cipher-test
  (testing "Failed decryption of caesar cipher!" (is (= ciphertext plaintext))))
