(ns set1.xor-cipher-test
  (:require [clojure.test :refer :all]
            [set1.xor-cipher :refer :all]))


(def inp "1b37373331363f78151b7f2b783431333d78397828372d363c78373e783a393b3736")

(deftest xor-cipher-test
  (testing "Fail!" (is (= (decrypt inp) "Cooking MC's like a pound of bacon"))))
