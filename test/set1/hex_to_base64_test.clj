(ns set1.hex-to-base64-test
  (:require [clojure.test :refer :all]
            [set1.hex-to-base64 :refer :all]))



(deftest hex-to-base64-test
  (def inp "49276d206b696c6c696e6720796f757220627261696e206c696b65206120706f69736f6e6f7573206d757368726f6f6d")
  (def out "SSdtIGtpbGxpbmcgeW91ciBicmFpbiBsaWtlIGEgcG9pc29ub3VzIG11c2hyb29t")
  (testing "" (is (= out (encode inp)))))
