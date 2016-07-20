(ns set2.validate-padding-test
  (:require [clojure.test :refer :all]
            [util.padding :as sut]))

;; Write a function that takes a plaintext, determines if it has valid PKCS#7 padding, and strips the padding off.

;; The string:

;; "ICE ICE BABY\x04\x04\x04\x04"
;; ... has valid padding, and produces the result "ICE ICE BABY".

;; The string:

;; "ICE ICE BABY\x05\x05\x05\x05"
;; ... does not have valid padding, nor does:

;; "ICE ICE BABY\x01\x02\x03\x04"
;; If you are writing in a language with exceptions, like Python or Ruby, make your function throw an exception on bad padding.

;; Crypto nerds know where we're going with this. Bear with us.


(deftest validate-padding-test
  (testing "Failed to validate padding"
    (is (sut/validate-padding (sut/pad-plaintext (repeat 5 0) 16 :pkcs7) 16 :pkcs7))
    (is (not (sut/validate-padding (concat (sut/pad-plaintext (repeat 5 0) 16 :pkcs7) [4]) 16 :pkcs7)))))
