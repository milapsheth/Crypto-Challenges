(ns set2.profile-parser
  (:require [clojure.string :as str]
            [util
             [aes :as aes]
             [random :as rand]
             [tools :as u]]))

(defn parse-profile
  "Show profile in human-readable form"
  [profile]
  (into {} (map #(str/split % #"=") (str/split profile #"&"))))


(defn encode-profile
  "Encodes profile in query like form"
  [profile]

  (when (some #(or (= % \&) (= % \=)) profile)
    (throw (Exception. "User email can't contain & or = in it")))

  (str "email=" profile "&uid=10&role=user"))


(def random-cipher-key (rand/byte-lst 16))

(defn encrypt-profile
  "Encrypt user profile using AES(ECB)"
  [profile]
  (-> (encode-profile profile)
      (u/str->bytes)
      (aes/encrypt random-cipher-key :ecb)))


(defn decrypt-profile
  "Decrypt user profile encrypted using AES(ECB)"
  [encrypted-profile]
  (-> (aes/decrypt encrypted-profile random-cipher-key :ecb)
      (u/bytes->str)
      (parse-profile)))
