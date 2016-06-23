(ns set2.break-ecb-simple-test
  (:require [set2.break-ecb-simple :as sut]
            [clojure.test :refer :all]
            [set1.aes :as aes]))


(def unknown-string (map int "UNKNOWN STRINGNO"))

(def random-cipher-key (repeatedly 16 #(rand-int 256)))

(defn oracle-encrypt
  [plaintext]
  (aes/encrypt (concat plaintext unknown-string) random-cipher-key :ecb))


(deftest break-ecb-simple-test
  (testing "Failed to break ECB mode(simple)"
    (is (= unknown-string
           (sut/break-ecb oracle-encrypt)))))
