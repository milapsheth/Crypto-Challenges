(ns set2.aes-oracle-test
  (:require [set2.aes-oracle :as oracle]
            [clojure.test :refer :all]))


(deftest ^:parallel aes-oracle-test
  (testing
      (let [plaintext (repeat (+ 50 (rand-int 50)) (int \A))]
        (-> (oracle/encrypt plaintext)
            ((fn [[mode cipher]]
               (is (= mode (oracle/detect-mode cipher)))))))))
