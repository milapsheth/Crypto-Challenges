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
   - [x] [ECB cut-and-paste](test/set2/profile_parser_test.clj)
   - [x] [Byte-at-a-time ECB decryption (Harder)](test/set2/break_ecb_harder_test.clj)
   - [x] [PKCS#7 padding validation](test/set2/validate_padding_test.clj)
   - [x] [CBC bitflipping attacks](test/set2/cbc_attack_test.clj)
* Set 3
   - [x] [The CBC padding oracle](test/set3/cbc_padding_oracle_test.clj)
   - [x] [Implement CTR, the stream cipher mode](test/set3/decrypt_ctr_test.clj)
   - [x] [Break fixed-nonce CTR mode using substitutions](test/set3/break_ctr_test.clj)
   - [x] [Break fixed-nonce CTR statistically](test/set3/break_ctr_test.clj)
   - [x] [Implement the MT19937 Mersenne Twister RNG](src/util/mersenne_twister.clj)
   - [ ] Crack an MT19937 seed
   - [x] [Clone an MT19937 RNG from its output](test/set3/break_mt.clj)
   - [ ] Create the MT19937 stream cipher and break it
* Set 4
   - [x] [Break "random access read/write" AES CTR](test/set4/break_edit_ctr.clj)
   - [x] [CTR bitflipping](test/set4/break_ctr_bitflip.clj)
   - [x] [Recover the key from CBC with IV=Key](test/set4/cbc_key_recovery_test.clj)
   - [x] [Implement a SHA-1 keyed MAC](test/set4/sha1_test.clj)
   - [x] [Break a SHA-1 keyed MAC using length extension](test/set4/sha1_length_ext_test.clj)
   - [ ] Break an MD4 keyed MAC using length extension
   - [x] [Implement and break HMAC-SHA1 with an artificial timing leak](test/set4/hmac_timing_attack_test.clj)
   - [ ] Break HMAC-SHA1 with a slightly less artificial timing leak
* Set 5
   - [x] [Implement Diffie-Hellman](test/set5/dh_test.clj)
   - [ ] Implement a MITM key-fixing attack on Diffie-Hellman with parameter injection
   - [ ] Implement DH with negotiated groups, and break with malicious "g" parameters
   - [ ] Implement Secure Remote Password (SRP)
   - [ ] Break SRP with a zero key
   - [ ] Offline dictionary attack on simplified SRP
   - [ ] Implement RSA
   - [ ] Implement an E=3 RSA Broadcast attack
* Set 6
* Set 7
* Set 8
