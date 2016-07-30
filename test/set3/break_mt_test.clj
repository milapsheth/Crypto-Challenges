(ns set3.break-mt-test
  (:require [set3.break-mt :as sut]
            [clojure.test :refer :all]
            [util.mersenne-twister :as mt]))


(defn gen-random-numbers
  []
  (do (repeatedly (rand-int 1248) mt/extract-number)
      (vec (repeatedly (+ 1248 (rand-int 1248)) mt/extract-number))))

(defn break-rng
  [nums]
  (sut/clone-state nums))


(defn confirm-broken-rng
  "Generate random numbers from source and clone state
  and confirm they are equal"
  []
  (let [x (rand-int 624)]
    (= (repeatedly (+ 624 x) mt/extract-number)
       (repeatedly (+ 624 x) sut/predict-next))))

(deftest break-mt-test
  (testing "Failed to break Mersenne twister"
    (mt/initialize-generator (System/currentTimeMillis))
    (break-rng (gen-random-numbers))
    (is (confirm-broken-rng))))


(deftest break-mt-test-hard
  (testing "Failed to break Mersenne twister"
    (dotimes [n 5]
      (mt/initialize-generator (System/currentTimeMillis))
      (gen-random-numbers)
      (dotimes [n 4]
        (break-rng (gen-random-numbers))
        (is (confirm-broken-rng))
        (gen-random-numbers)))))
