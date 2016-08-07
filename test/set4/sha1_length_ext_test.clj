(ns set4.sha1-length-ext-test
  (:require [set4.sha1-length-ext :as sut]
            [clojure.test :refer :all]
            [util.tools :as u]
            [util.random :as rand]
            [util.sha1 :as sha1]))


(def secret-key (rand/byte-lst (rand-int 128)))


(defn get-auth-msg-hash
  [msg]
  (sha1/hash (concat secret-key msg)))


(defn validate
  "Validate message using SHA1 hash"
  [msg msg-hash]
  (= (sha1/hash (concat secret-key msg))
     msg-hash))


(deftest break-sha1-length-ext-test
  (testing "Could forge SHA1 hash with length extension"
    (is (let [msg (u/str->bytes "comment1=cooking%20MCs;userdata=foo;comment2=%20like%20a%20pound%20of%20bacon")
              msg-to-add (u/str->bytes ";admin=true")
              [received-msg received-hash] (sut/forge-hash msg (get-auth-msg-hash msg) msg-to-add validate)]
          (validate received-msg received-hash)))))


(deftest ^:parallel break-sha1-length-ext-test-hard
  (testing "Could forge SHA1 hash with length extension"
    (dotimes [n 10]
      (let [msg (rand/byte-lst (rand-int 256))
            msg-to-add (rand/byte-lst (rand-int 256))
            [received-msg received-hash] (sut/forge-hash msg (get-auth-msg-hash msg) msg-to-add validate)]
        (is (validate received-msg received-hash))))))
