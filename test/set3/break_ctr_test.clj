(ns set3.break-ctr-test
  (:require [set3.break-ctr :as sut]
            [clojure.test :refer :all]
            [util.tools :as u]
            [util.random :as rand]
            [util.aes :as aes]
            [clojure.string :as str]
            [clojure.java.io :as io]))


(def plaintexts (map u/base64-to-byte' (str/split-lines (slurp (io/resource "set3/more_ctr_plaintexts.txt")))))


(def cipher-key (rand/byte-lst 16))

(def nonce (repeat 8 0))

(defn encrypt-text
  [plaintext]
  (aes/encrypt plaintext cipher-key :ctr nonce))

(def ciphertexts (map encrypt-text plaintexts))


(defn check-broken-plaintext-success
  [pts]
  (->> plaintexts
       (map #(count (filter (fn [[f t]] (not= f t)) (map vector %1 %2))) pts)
       (reduce + 0)
       ;; If >75% of text matched, then breaking worked quite well
       (> (/ (reduce #(+ %1 (count %2)) 0 plaintexts) 25))))


(deftest ^:parallel break-ctr-test
  (testing "Failed to break CTR ciphertext"
    (is (check-broken-plaintext-success (sut/break-ctr ciphertexts)))))
