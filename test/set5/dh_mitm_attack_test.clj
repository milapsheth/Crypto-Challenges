(ns set5.dh-mitm-attack-test
  (:require [clojure.test :refer :all]
            [set5.dh-mitm-attack :as sut]
            [util.random :as rand]
            [util.tools :as u]))

(def p (read-string "0xffffffffffffffffc90fdaa22168c234c4c6628b80dc1cd129024e088a67cc74020bbea63b139b22514a08798e3404ddef9519b3cd3a431b302b0a6df25f14374fe1356d6d51c245e485b576625e7ec6f44c42e9a637ed6b0bff5cb6f406b7edee386bfb5a899fa5ae9f24117c4b1fe649286651ece45b3dc2007cb8a163bf0598da48361c55d39a69163fa8fd24cf5f83655d23dca3ad961c62f356208552bb9ed529077096966d670c354e4abc9804f1746c08ca237327ffffffffffffffff"))

(def g 2)


(def alice (sut/create-peer))

(def bob (sut/create-peer))


(defn test-normal-msg-send
  [alice bob]
  (let [msg (rand/byte-lst (rand/rand-num 50 100))]
    (sut/send-msg alice bob msg)
    (= (:msg @alice)
       (:msg @bob))))


(defn test-establish-keys
  [alice bob]
  (sut/establish-keys alice bob p g)
  (= (:aes-key @alice)
     (:aes-key @bob)))


(deftest dh-mitm-attack-normal-test
  (testing "Failed normal message communication"
    (let [alice (sut/create-peer)
          bob (sut/create-peer)]
      (dotimes [n 4]
        (is (test-establish-keys (if (even? n) alice bob)
                                 (if (even? n) bob alice)))
        (dotimes [n 4]
          (is (test-normal-msg-send (if (even? n) alice bob)
                                    (if (even? n) bob alice))))))))


(defn check-registered-public-key
  [peer]
  (= (:public-key @peer)
     (get @sut/listener [:public-key (:id @peer)])))


(defn test-attacked-msg
  [alice bob]
  (let [forged-msg (rand/byte-lst (rand/rand-num 20 50))
        msg-to-send (rand/byte-lst (rand/rand-num 20 50))]
    (sut/dh-mitm-attack forged-msg)
    (sut/send-msg alice bob msg-to-send)
    (and (= (:msg @bob)
            forged-msg)
         (= (:msg @alice)
            msg-to-send))))


(deftest dh-mitm-attack-normal-test-actual
  (testing "Failed to break communication"
    (let [alice (sut/create-peer)
          bob (sut/create-peer)]
      (sut/dh-mitm-attack)
      (is (test-establish-keys alice bob))
      (is (check-registered-public-key alice))
      (is (check-registered-public-key bob))
      (dotimes [n 4]
        (is (test-attacked-msg (if (even? n) alice bob)
                               (if (even? n) bob alice))))
      (reset! sut/channel identity))))
