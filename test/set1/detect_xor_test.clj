(ns set1.detect-xor-test
  (:require [clojure.test :refer :all]
            [set1.detect-xor :refer :all]))


;; Detect the encrypted message

(deftest detect-xor-test
  (testing "Detected wrong message"
    (is (= (detect-encrypted) "Now that the party is jumping\n"))))
