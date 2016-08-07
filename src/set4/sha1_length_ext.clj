(ns set4.sha1-length-ext
  (:require [util.sha1 :as sha1]
            [util.tools :as u]))


(def MAX-KEYLEN 128)

(defn forge-hash-with-guessed-keylen
  "Forge SHA1 hash with a prefix secret key length guess"
  [msg msg-hash-init new-msg keylen]
  (let [forged-msg (-> (concat (repeat keylen 0) msg)
                       sha1/pre-process
                       (#(drop keylen %))
                       (concat new-msg))]
    [forged-msg
     (sha1/hash new-msg msg-hash-init (+ keylen (count forged-msg)))]))


(defn forge-hash
  "Forge SHA1 hash for any message appended to original text"
  [msg msg-hash new-msg valid?]

  (let [msg-hash-init (mapv #(u/bytes->int % :big) (u/partition' 4 msg-hash))]
    (loop [keylen? 0]
      (if (<= keylen? MAX-KEYLEN)

        (let [[forged-msg forged-hash] (forge-hash-with-guessed-keylen msg msg-hash-init new-msg keylen?)]
          (if (valid? forged-msg forged-hash)
            ;; New message = msg + glue-padding + new-msg
            [forged-msg forged-hash]
            (recur (inc keylen?))))
        (throw (Exception. "Could not forge SHA1 hash"))))))
