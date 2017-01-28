(ns util.hmac-test
  (:require [util.hmac :as sut]
            [clojure.test :refer :all]
            [util.sha1 :as sha1]
            [util.tools :as u]))


(deftest hmac-test
  (testing "HMAC test failure"
    (is (= (sut/hmac sha1/sha1 64 () ())
           (u/str->bytes (u/hexstr->str "fbdb1d1b18aa6c08324b7d64b71fb76370690e1d"))))))


(deftest hmac-test2
  (testing "HMAC test failure"
    (is (= (sut/hmac sha1/sha1 64 (u/str->bytes "key") (u/str->bytes "The quick brown fox jumps over the lazy dog"))
           (u/str->bytes (u/hexstr->str "de7c9b85b8b78aa6bc8a7a36f70a90701c9db4d9"))))))
