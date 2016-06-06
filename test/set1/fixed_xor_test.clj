(ns set1.fixed-xor-test
  (:require [clojure.test :refer :all]
            [set1.fixed-xor :refer :all]))


(def inp1 "1c0111001f010100061a024b53535009181c")

(def inp2 "686974207468652062756c6c277320657965")

(def out "746865206b696420646f6e277420706c6179")


(deftest fixed-xor-test
  (testing ""
    (is (= out (fixed-xor inp1 inp2)))))
