(ns util.tools-test
  (:require [util.tools :as sut]
            [clojure.test :refer :all]
            [util.random :as rand]))



(deftest conversion-test
  (testing
      (is (every? true?
                  [(sut/int->bytes (sut/bytes->int (rand/byte-lst 4)))
                   (sut/bytes->int (sut/int->bytes (rand-int 0xFFFFFFFF)))
                   (sut/long->bytes (sut/bytes->long (rand/byte-lst 8)))
                   (sut/bytes->long (sut/long->bytes (rand-int 0xFFFFFFFFFFFFFFFF)))]))))
