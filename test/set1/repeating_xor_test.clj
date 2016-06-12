(ns set1.repeating-xor-test
  (:require [clojure.test :refer :all]
            [set1.repeating-xor :refer :all]
            [util.conv :as u]))


(def plaintext "Burning 'em, if you ain't quick and nimble
I go crazy when I hear a cymbal")

(def cipher-key "ICE")

(def ciphertext (apply str (u/hexstr-to-str "0b3637272a2b2e63622c2e69692a23693a2a3c6324202d623d63343c2a26226324272765272a282b2f20430a652e2c652a3124333a653e2b2027630c692b20283165286326302e27282f")))


(defn encrypt'
  [data key]
  (apply str (map char (encrypt (map int data) (map int key)))))


(deftest repeating-xor-test
  (testing "Didn't encrypt well"
    (is (= ciphertext (encrypt' plaintext cipher-key)))))
