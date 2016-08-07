(ns set4.ctr-bitflip-test
  (:require [clojure
             [string :as str]
             [test :refer :all]]
            [set4.ctr-bitflip :as sut]
            [util
             [aes :as aes]
             [random :as rand]
             [tools :as u]]))

;; CHALLENGE 26

;; CTR bitflipping
;; There are people in the world that believe that CTR resists bit flipping attacks of the kind to which CBC mode is susceptible.

;; Re-implement the CBC bitflipping exercise from earlier to use CTR mode instead of CBC mode. Inject an "admin=true" token.



(def random-cipher-key (rand/byte-lst 16))

(def random-nonce (rand/byte-lst 8))

(def prefix (mapv int "comment1=cooking%20MCs;userdata="))

(def suffix (mapv int ";comment2=%20like%20a%20pound%20of%20bacon"))


(defn encrypt-cookie
  [userdata]
  (when (some #(or (= (int \;) %) (= (int \=) %)) userdata)
    (throw (Exception. "Invalid userdata. Should not contain ';' or '='")))
  
  (aes/encrypt (concat prefix
                       userdata
                       suffix)
               random-cipher-key
               :ctr random-nonce))


(defn decrypt-cookie
  [ciphertext]
  (aes/decrypt ciphertext random-cipher-key :ctr random-nonce))


(defn parse-cookie
  [cookie]
  (reduce #(conj %1 (str/split %2 #"="))
          {}
          (filter #(not= % "") (str/split (u/bytes->str cookie) #";"))))

(defn is-admin?
  [cookie]
  (= "true" (get (parse-cookie cookie) "admin")))


(deftest ctr-bitflip-test
  (testing "Failed to attack CTR using bitflipping"
    (let [malicious-text (u/str->bytes "padding;admin=true;")]
      (is (->> malicious-text
               (sut/ctr-bitflip-attack encrypt-cookie)
               decrypt-cookie
               is-admin?)))))


(deftest ctr-bitflip-test2
  (testing "Failed to attack CTR using bitflipping"
    (let [malicious-text (u/str->bytes "padding;priv=root;")]
      (is (-> (sut/ctr-bitflip-attack encrypt-cookie malicious-text)
              decrypt-cookie
              parse-cookie
              (get "priv" nil)
              (= "root"))))))
