(ns set3.decrypt-ctr-test
  (:require [clojure.test :refer :all]
            [util
             [aes :as aes]
             [tools :as u]]))

(def ciphertext (u/base64-to-byte' "L77na/nrFsKvynd6HzOoG7GHTLXsTVu9qvY/2syLXzhPweyyMTJULu/6/kXX0KSvoOLSFQ=="))

(def cipher-key (map int "YELLOW SUBMARINE"))

(def nonce (repeat 8 0))

(def plaintext (map int "Yo, VIP Let's kick it Ice, Ice, baby Ice, Ice, baby "))


(deftest decrypt-ctr-test
  (testing "Failed to decrypt AES under CTR mode"
    (is (= plaintext
           (aes/decrypt ciphertext cipher-key :ctr nonce)))))
