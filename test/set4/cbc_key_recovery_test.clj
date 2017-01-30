(ns set4.cbc-key-recovery-test
  (:require [set4.cbc-key-recovery :as sut]
            [clojure.test :refer :all]
            [util.tools :as u]
            [util.random :as rand]
            [util.aes :as aes]
            [clojure.string :as str]))


(def random-cipher-key (rand/byte-lst 16))

(def random-iv random-cipher-key)

(def prefix (map int "comment1=cooking%20MCs;userdata="))

(def suffix (map int ";comment2=%20like%20a%20pound%20of%20bacon"))

(defn encrypt-cookie
  [userdata]
  (when (some #(or (= (int \;) %) (= (int \=) %)) userdata)
    (throw (Exception. "Invalid userdata. Should not contain ; or =")))
  
  (aes/encrypt (concat prefix
                       userdata
                       suffix)
               random-cipher-key
               :cbc random-iv))


(defn decrypt-cookie
  [ciphertext]
  (let [plaintext (aes/decrypt ciphertext random-cipher-key :cbc random-iv)]
    (if (first (filter #(> % 127) plaintext))
      (u/raise (u/bytes->str plaintext))
      plaintext)))


(defn parse-cookie
  [cookie]
  (reduce #(conj %1 (str/split %2 #"=")) {}
          (filter #(not= % "") (str/split (u/bytes->str cookie) #";"))))


(deftest cbc-key-recovery-test
  (testing "Failed to extract CBC key"
    (is (= random-cipher-key
           (sut/cbc-key-recovery encrypt-cookie decrypt-cookie)))))
