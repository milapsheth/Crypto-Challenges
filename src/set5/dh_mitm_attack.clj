(ns set5.dh-mitm-attack
  (:require [util
             [aes :as aes]
             [math :as math]
             [random :as rand]
             [sha1 :as sha1]
             [tools :as u]]))


(def MAX-KEY-BYTE-LEN 192)


(defn create-peer []
  (let [private-key (u/bytes->number (rand/byte-lst MAX-KEY-BYTE-LEN))]
    (atom {:id (rand/rand-num),
           :private-key private-key})))


(defn create-public-key
  [peer p g]
  (swap! peer assoc :p p :g g :public-key (math/pow g (:private-key @peer) p)))

(defn encrypt-with-key
  [peer m]
  (math/pow m (:private-key @peer) (:p @peer)))


(defn register-aes-key
  "Process registration request and response
  Registration request: create symmetric AES key from DH keypair
  and send public key
  Registration response: create symmetric AES key from obtained public key"
  ([peer msg]
   (cond
     (:reg-request msg) (let [[p g peer-public-key] (:reg-request msg)]
                          (create-public-key peer p g)
                          (register-aes-key peer p peer-public-key)
                          {:reg-response (:public-key @peer),
                           :id (:id @peer), :remote-id (:id msg)})
     (:reg-response msg) (do (register-aes-key peer (:p @peer) (:reg-response msg))
                             nil)
     :else (u/raise "Invalid message")))
  
  ([peer p peer-public-key]
   (swap! peer (fn [{private-key :private-key :as mp}]
                 (assoc mp
                        :remote-public-key peer-public-key
                        :aes-key (->> (math/pow peer-public-key private-key p)
                                      u/number->bytes
                                      sha1/sha1
                                      (take 16)))))))


(def channel (atom identity))


(defn transport-msg
  [msg]
  (@channel msg))


(defn create-reg-request
  [peer remote-id p g]
  (create-public-key peer p g)
  {:reg-request [p, g, (:public-key @peer)],
   :id (:id @peer), :remote-id remote-id})


(defn establish-keys
  [peer1 peer2 p g]
  (->> ;; Create registration request
   (create-reg-request peer1 (:id @peer2) p g)
   transport-msg
   (register-aes-key peer2)
   transport-msg
   (register-aes-key peer1)))


(defn encrypt-msg
  [peer remote-id msg]
  (let [msg-to-send (aes/encrypt msg (:aes-key @peer) :cbc)]
    (swap! peer assoc :msg msg)
    {:msg msg-to-send,
     :id (:id @peer),
     :remote-id remote-id}))


(defn decrypt-msg
  [peer {cipher :msg id :id}]
  (let [msg (aes/decrypt cipher (:aes-key @peer) :cbc)]
    (swap! peer #(assoc % :msg msg))
    msg))


(defn send-msg
  [sender receiver msg]
  (->> (encrypt-msg sender (:id @receiver) msg)
       transport-msg
       (decrypt-msg receiver)))

;; Structure
;; {id => {p, g, msg, aes-key}}
(def listener (atom nil))


(defn modify-msg
  [listener {cipher :msg id :id}]
  (let [plaintext (aes/decrypt cipher (:aes-key @listener) :cbc)
        ciphertext (aes/encrypt (:msg @listener) (:aes-key @listener) :cbc)]
    (swap! listener assoc [:msg id] plaintext)
    {:msg ciphertext :id id}))


(defn modify-reg-response
  [listener {msg :reg-response id :id :as mp}]
  (let [B msg]
    (swap! listener assoc [:public-key id] B
           :aes-key (->> (u/number->bytes 0) ;; AES Key after modification
                         sha1/sha1
                         (take 16)))
    {:reg-response (:p @listener) :id id}))

(defn modify-reg-request
  [listener {[p g A] :reg-request, id :id}]
  (let [malicious-g (or (:g @listener) g)]
    (swap! listener assoc :p p :g malicious-g
           [:public-key id] A)
    {:reg-request [p malicious-g p], :id id}))


(defn msg-listener
  [msg]
  (cond
    (:msg msg) (modify-msg listener msg)
    (:reg-request msg) (modify-reg-request listener msg)
    (:reg-response msg) (modify-reg-response listener msg)
    :else (u/raise "Cannot decipher message")))


(defn dh-mitm-attack
  "Add attacker to the channel.
  Add message to insert"
  ([]
   (reset! listener @(create-peer))
   (reset! channel msg-listener))
  ([msg & args] (apply swap! listener assoc :msg msg args)))

(defn dh-get-listener
  []
  listener)
