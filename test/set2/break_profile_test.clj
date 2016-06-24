(ns set2.break-profile-test
  (:require [set2.profile-parser :as profile]
            [clojure.test :refer :all]
            [clojure.string :as str]))


;; ECB cut-and-paste

;; Write a k=v parsing routine, as if for a structured cookie. The routine should take:

;; foo=bar&baz=qux&zap=zazzle
;; ... and produce:

;; {
;;   foo: 'bar',
;;   baz: 'qux',
;;   zap: 'zazzle'
;; }
;; (you know, the object; I don't care if you convert it to JSON).

;; Now write a function that encodes a user profile in that format, given an email address. You should have something like:

;; profile_for("foo@bar.com")
;; ... and it should produce:

;; {
;;   email: 'foo@bar.com',
;;   uid: 10,
;;   role: 'user'
;; }
;; ... encoded as:

;; email=foo@bar.com&uid=10&role=user
;; Your "profile_for" function should not allow encoding metacharacters (& and =). Eat them, quote them, whatever you want to do, but don't let people set their email address to "foo@bar.com&role=admin".

;; Now, two more easy functions. Generate a random AES key, then:

;; Encrypt the encoded user profile under the key; "provide" that to the "attacker".
;; Decrypt the encoded user profile and parse it.
;; Using only the user input to profile_for() (as an oracle to generate "valid" ciphertexts) and the ciphertexts themselves, make a role=admin profile.


;; Take advantage of PKCS#7 padding to get separate ciphertexts
;; for email and "admin"

(def block-size 16)

(def profile1 (str "break.thisadmin" (str/join (repeat 11 \o13))))

(def profile2 "breaking.this")

(defn make-admin []
  (profile/decrypt-profile
   (concat (drop-last block-size (profile/encrypt-profile profile2))
           (take block-size (drop block-size
                                  (profile/encrypt-profile profile1))))))


(deftest break-profile-test
  (testing "Failed to make user admin"
    (is (= "admin"
           (get (make-admin) "role")))))
