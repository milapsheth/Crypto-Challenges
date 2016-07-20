(ns set3.cbc-padding-oracle-test
  (:require [clojure.test :refer :all]
            [set3.cbc-padding-oracle :as sut]
            [util
             [aes :as aes]
             [random :as rand]
             [tools :as u]]))

;; The CBC padding oracle

;; This is the best-known attack on modern block-cipher cryptography.

;; Combine your padding code and your CBC code to write two functions.

;; The first function should select at random one of the following 10 strings:

;; MDAwMDAwTm93IHRoYXQgdGhlIHBhcnR5IGlzIGp1bXBpbmc=
;; MDAwMDAxV2l0aCB0aGUgYmFzcyBraWNrZWQgaW4gYW5kIHRoZSBWZWdhJ3MgYXJlIHB1bXBpbic=
;; MDAwMDAyUXVpY2sgdG8gdGhlIHBvaW50LCB0byB0aGUgcG9pbnQsIG5vIGZha2luZw==
;; MDAwMDAzQ29va2luZyBNQydzIGxpa2UgYSBwb3VuZCBvZiBiYWNvbg==
;; MDAwMDA0QnVybmluZyAnZW0sIGlmIHlvdSBhaW4ndCBxdWljayBhbmQgbmltYmxl
;; MDAwMDA1SSBnbyBjcmF6eSB3aGVuIEkgaGVhciBhIGN5bWJhbA==
;; MDAwMDA2QW5kIGEgaGlnaCBoYXQgd2l0aCBhIHNvdXBlZCB1cCB0ZW1wbw==
;; MDAwMDA3SSdtIG9uIGEgcm9sbCwgaXQncyB0aW1lIHRvIGdvIHNvbG8=
;; MDAwMDA4b2xsaW4nIGluIG15IGZpdmUgcG9pbnQgb2g=
;; MDAwMDA5aXRoIG15IHJhZy10b3AgZG93biBzbyBteSBoYWlyIGNhbiBibG93
;; ... generate a random AES key (which it should save for all future encryptions), pad the string out to the 16-byte AES block size and CBC-encrypt it under that key, providing the caller the ciphertext and IV.

;; The second function should consume the ciphertext produced by the first function, decrypt it, check its padding, and return true or false depending on whether the padding is valid.

;; What you're doing here.
;; This pair of functions approximates AES-CBC encryption as its deployed serverside in web applications; the second function models the server's consumption of an encrypted session token, as if it was a cookie.

;; It turns out that it's possible to decrypt the ciphertexts provided by the first function.

;; The decryption here depends on a side-channel leak by the decryption function. The leak is the error message that the padding is valid or not.

;; You can find 100 web pages on how this attack works, so I won't re-explain it. What I'll say is this:

;; The fundamental insight behind this attack is that the byte 01h is valid padding, and occur in 1/256 trials of "randomized" plaintexts produced by decrypting a tampered ciphertext.

;; 02h in isolation is not valid padding.

;; 02h 02h is valid padding, but is much less likely to occur randomly than 01h.

;; 03h 03h 03h is even less likely.

;; So you can assume that if you corrupt a decryption AND it had valid padding, you know what that padding byte is.

;; It is easy to get tripped up on the fact that CBC plaintexts are "padded". Padding oracles have nothing to do with the actual padding on a CBC plaintext. It's an attack that targets a specific bit of code that handles decryption. You can mount a padding oracle on any CBC block, whether it's padded or not.


(def strings (mapv u/base64-to-byte'
                   ["MDAwMDAwTm93IHRoYXQgdGhlIHBhcnR5IGlzIGp1bXBpbmc="
                    "MDAwMDAxV2l0aCB0aGUgYmFzcyBraWNrZWQgaW4gYW5kIHRoZSBWZWdhJ3MgYXJlIHB1bXBpbic="
                    "MDAwMDAyUXVpY2sgdG8gdGhlIHBvaW50LCB0byB0aGUgcG9pbnQsIG5vIGZha2luZw=="
                    "MDAwMDAzQ29va2luZyBNQydzIGxpa2UgYSBwb3VuZCBvZiBiYWNvbg=="
                    "MDAwMDA0QnVybmluZyAnZW0sIGlmIHlvdSBhaW4ndCBxdWljayBhbmQgbmltYmxl"
                    "MDAwMDA1SSBnbyBjcmF6eSB3aGVuIEkgaGVhciBhIGN5bWJhbA=="
                    "MDAwMDA2QW5kIGEgaGlnaCBoYXQgd2l0aCBhIHNvdXBlZCB1cCB0ZW1wbw=="
                    "MDAwMDA3SSdtIG9uIGEgcm9sbCwgaXQncyB0aW1lIHRvIGdvIHNvbG8="
                    "MDAwMDA4b2xsaW4nIGluIG15IGZpdmUgcG9pbnQgb2g="
                    "MDAwMDA5aXRoIG15IHJhZy10b3AgZG93biBzbyBteSBoYWlyIGNhbiBibG93"]))


(def cipher-key (rand/byte-lst 16))

(def BLOCK-SIZE 16)


(defn oracle-encrypt
  [plaintext]
  (let [iv (rand/byte-lst 16)]
    [iv
     (aes/encrypt (concat plaintext (rand-nth strings)) cipher-key :cbc iv)]))


(defn valid-padding?
  [cipher-key]
  (fn [iv ciphertext]
    (try (do (aes/decrypt ciphertext cipher-key :cbc iv)
             true)
         (catch Exception e
           false))))


(defn decrypt-all-strings
  []
  (let [plaintext (map int "plaintext")
        plaintext-len (count plaintext)
        padding-oracle (valid-padding? cipher-key)]
    (loop [set' #{}]
      (if (= (count set') (count strings))
        set'
        (let [[iv ciphertext] (oracle-encrypt plaintext)]
          (->> (sut/cbc-padding-oracle-attack iv ciphertext BLOCK-SIZE padding-oracle)
               (drop plaintext-len)
               (conj set')
               (recur)))))))


;; Challenge 17
#_
(deftest cbc-padding-oracle-test
  (testing "Failed to mount padding oracle attack on CBC"
    (is (= (set strings)
           (decrypt-all-strings)))))


(deftest cbc-padding-oracle-basic-test
  (testing "Failed to mount basic padding oracle attack on CBC"
    (let [plaintext (rand/byte-lst 35)
          cipher-key (rand/byte-lst 16)
          iv (rand/byte-lst 16)
          ciphertext (aes/encrypt plaintext cipher-key :cbc iv)]
      (is (= plaintext
             (sut/cbc-padding-oracle-attack iv ciphertext 16 (valid-padding? cipher-key)))))))
