# Crypto-Challenges

[![Build Status](https://travis-ci.org/milapsheth/Crypto-Challenges.svg?branch=master)](https://travis-ci.org/milapsheth/Crypto-Challenges)

My attempt at completing [the cryptopals crypto challenges](http://cryptopals.com/) and planning to learn Clojure on the side.

Run `lein parallel-test` to run all challenges.

## Completed challenges

* Set 1
   - [x] [Convert hex to base64](test/set1/hex_to_base64_test.clj)
   - [x] [Fixed XOR](test/set1/fixed_xor_test.clj)
   - [x] [Single XOR-byte cipher](test/set1/xor_cipher_test.clj)
   - [x] [Detect single-character XOR](test/set1/detect_xor_test.clj)
   - [x] [Repeating XOR-cipher](test/set1/repeating_xor_test.clj)
   - [x] [Decrypting repeating key XOR](test/set1/decrypt_vigenere_test.clj)
   - [x] [AES in ECB mode](test/set1/decrypt_aes_test.clj)
   - [x] [Detect AES in ECB mode](test/set1/detect_aes_test.clj)
* Set 2
   - [x] [Implement PKCS#7 padding](test/set2/padding_test.clj)
   - [x] [Implement CBC mode](test/set2/decrypt_cbc_test.clj)
   - [x] [An ECB/CBC detection oracle](test/set2/aes_oracle_test.clj)
   - [x] [Byte-at-a-time ECB decryption (Simple)](test/set2/break_ecb_simple_test.clj)
   - [x] ECB cut-and-paste
   - [ ] Byte-at-a-time ECB decryption (Harder)
   - [ ] PKCS#7 padding validation
   - [ ] CBC bitflipping attacks
* Set 3
* Set 4
* Set 5
* Set 6
* Set 7
* Set 8
