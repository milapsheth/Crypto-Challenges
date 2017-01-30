(ns set2.cbc-attack-test
  (:require [clojure
             [string :as str]
             [test :refer :all]]
            [set2.cbc-attack :as sut]
            [util
             [aes :as aes]
             [random :as rand]
             [tools :as u]]))

;; -------------------------
;; Utilities
;; -------------------------

(def random-cipher-key (rand/byte-lst 16))
(def random-iv (rand/byte-lst 16))

(def msg-prefix "comment1=cooking%20MCs;userdata=")
(def msg-suffix ";comment2=%20like%20a%20pound%20of%20bacon")


(defn encrypt-cookie
  [userdata]
  (when (some #(or (= \; %) (= \= %)) userdata)
    (throw (Exception. "Invalid userdata. Should not contain ; or =")))
  
  (aes/encrypt (map int (concat msg-prefix userdata msg-suffix))
               random-cipher-key
               :cbc random-iv))


(defn decrypt-cookie
  [ciphertext]
  (u/bytes->str (aes/decrypt ciphertext random-cipher-key :cbc random-iv)))


(defn parse-cookie
  [cookie]
  (reduce #(conj %1 (str/split %2 #"=")) {}
          (filter #(not= % "") (str/split cookie #";"))))


(defn oracle-encrypt
  [text]
  (encrypt-cookie (u/bytes->str text)))


(defn is-admin?
  [cookie]
  (= "true" (get (parse-cookie cookie) "admin")))


;; -------------------------
;; Tests
;; -------------------------

(deftest cbc-attack-test
  (testing "Failed attacking CBC"
    (is (-> ";admin=true;"
            (sut/attack-cbc oracle-encrypt)
            decrypt-cookie
            is-admin?))))


(deftest cbc-attack-test2
  (testing "Falied attacking CBC"
    (is (-> ";priv=root;"
            (sut/attack-cbc oracle-encrypt)
            decrypt-cookie
            parse-cookie
            (get "priv")
            (= "root")))))
