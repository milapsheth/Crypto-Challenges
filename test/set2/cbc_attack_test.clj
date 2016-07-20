(ns set2.cbc-attack-test
  (:require [clojure.test :refer :all]
            [set2.cbc-attack :as sut]
            [util.tools :as u]))

(defn oracle-encrypt
  [text]
  (sut/encrypt-cookie (u/bytes->str text)))

(defn is-admin?
  [cookie]
  (= "true" (get (sut/parse-cookie cookie) "admin")))


(deftest cbc-attack-test
  (testing "Failed attacking CBC"
    (is (-> ";admin=true;"
            (sut/attack-cbc oracle-encrypt)
            sut/decrypt-cookie
            is-admin?))))


(deftest cbc-attack-test2
  (testing "Falied attacking CBC"
    (is (-> ";priv=root;"
            (sut/attack-cbc oracle-encrypt)
            sut/decrypt-cookie
            sut/parse-cookie
            (get "priv")
            (= "root")))))
