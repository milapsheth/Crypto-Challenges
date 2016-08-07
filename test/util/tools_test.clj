(ns util.tools-test
  (:require [util.tools :as sut]
            [clojure.test :refer :all]
            [util.random :as rand]))



(deftest conversion-test
  (testing
      (is (every? true?
                  [(let [num (rand/byte-lst 4)]
                     (= num (sut/int->bytes (sut/bytes->int num))))
                   (let [x (rand-int 0xFFFFFFF)]
                     (= x (sut/bytes->int (sut/int->bytes x))))
                   (let [num (rand/byte-lst 8)]
                     (= num (sut/long->bytes (sut/bytes->long num))))
                   (let [x (rand/rand-long 0xFFFFFFFFFFFFFFF)]
                     (= x (sut/bytes->long (sut/long->bytes x))))]))))
