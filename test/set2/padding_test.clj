(ns set2.padding-test
  (:require [clojure.test :refer :all]
            [util.padding :refer :all]))

(def plaintext (seq "YELLOW SUBMARINE"))


(deftest ^:parallel padding-test
  (testing "Failed to pad correctly"
    (is (= plaintext
           (-> plaintext
               (pad-plaintext 16 :pkcs7)
               (unpad-plaintext 16 :pkcs7))))))

(deftest ^:parallel padding-test2
  (testing "Failed padding test"
    (is (= (map int "YELLOW SUBMARINE\04\04\04\04")
           (-> (map int plaintext)
               (pad-plaintext 20 :pkcs7))))))
