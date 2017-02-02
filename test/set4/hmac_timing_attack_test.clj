(ns set4.hmac-timing-attack-test
  (:require [set4.hmac-timing-attack :as sut]
            [clojure.test :refer :all]
            [util.hmac :as hmac]
            [util.sha1 :as sha1]
            [util.random :as rand]
            [util.tools :as u]))


;; CHALLENGE 31
;; Implement and break HMAC-SHA1 with an artificial timing leak

;; The psuedocode on Wikipedia should be enough. HMAC is very easy.

;; Using the web framework of your choosing (Sinatra, web.py, whatever), write a tiny application that has a URL that takes a "file" argument and a "signature" argument, like so:

;; http://localhost:9000/test?file=foo&signature=46b4ec586117154dacd49d664e5d63fdc88efb51
;; Have the server generate an HMAC key, and then verify that the "signature" on incoming requests is valid for "file", using the "==" operator to compare the valid MAC for a file with the "signature" parameter (in other words, verify the HMAC the way any normal programmer would verify it).

;; Write a function, call it "insecure_compare", that implements the == operation by doing byte-at-a-time comparisons with early exit (ie, return false at the first non-matching byte).

;; In the loop for "insecure_compare", add a 50ms sleep (sleep 50ms after each byte).

;; Use your "insecure_compare" function to verify the HMACs on incoming requests, and test that the whole contraption works. Return a 500 if the MAC is invalid, and a 200 if it's OK.

;; Using the timing leak in this application, write a program that discovers the valid MAC for any file.


(def mac-key (rand/byte-lst (rand/rand-num 16 32)))

(defn insecure-compare
  [arr1 arr2]
  (cond
    (not= (first arr1) (first arr2)) false
    :else (do (Thread/sleep 30)
              (recur (rest arr1) (rest arr2)))))


(defn sign-file
  [file]
  (hmac/hmac sha1/sha1 64 mac-key file))

(defn verify
  [file signature]
  (let [mac (sign-file file)]
    (insecure-compare mac signature)))


;; Only check the first few bytes since it takes a while
(deftest ^:parallel hmac-timing-attack-test
  (testing "Failed to forge HMAC mac"
    (let [msg-to-forge (rand/byte-lst 5)
          correct-signature (sign-file msg-to-forge)
          forged-signature (sut/hmac-timing-attack-lazy verify msg-to-forge)]
      (is (= (take 4 forged-signature)
             (take 4 correct-signature))))))
