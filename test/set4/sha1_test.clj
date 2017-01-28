(ns set4.sha1-test
  (:require [clojure.test :refer :all]
            [util.sha1 :as sha1]
            [util.tools :as u]))


(deftest sha1-test1
  (testing "Wrong SHA1 hash"
    (is (= (sha1/sha1 (u/str->bytes "The quick brown fox jumps over the lazy cog"))
           (map int (u/hexstr->str "de9f2c7fd25e1b3afad3e85a0bd17d9b100db4b3"))))))


(deftest sha1-test2
  (testing "Wrong SHA1 hash"
    (is (= (sha1/sha1 (u/str->bytes "The quick brown fox jumps over the lazy dog"))
           (map int (u/hexstr->str "2fd4e1c67a2d28fced849ee1bb76e7391b93eb12"))))))


(deftest sha1-test3
  (testing "Wrong SHA1 hash"
    (is (= (sha1/sha1 (u/str->bytes ""))
           (map int (u/hexstr->str "da39a3ee5e6b4b0d3255bfef95601890afd80709"))))))
