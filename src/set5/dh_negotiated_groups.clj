(ns set5.dh-negotiated-groups
  (:require [set5.dh-mitm-attack :refer
             [channel create-peer create-public-key create-reg-request register-aes-key]]
            [util.aes :as aes]
            [util.sha1 :as sha1]
            [util.tools :as u]))


;; Structure
;; {id => {id, p, g, public-key, private-key, aes-key, msg}}
(def attacker (atom nil))


(defn modify-reg-request
  "Modify registation request with MITM peer"
  [attacker {[p g A] :reg-request, id :id, remote-id :remote-id :as msg}]
  (let [malicious-g (or (:g @attacker) g)
        malicious-p (or (:p @attacker) p)
        peer (create-peer)
        remote-peer (create-peer)]

    (register-aes-key peer msg)
    (create-public-key remote-peer malicious-p malicious-g)

    (swap! attacker assoc [:id id] peer
           [:id remote-id] remote-peer)

    {:reg-request [malicious-p malicious-g (:public-key @remote-peer)],
     :id id, :remote-id remote-id}))


(defn modify-reg-response
  "Modify registration response with MITM peer"
  [attacker {id :id, remote-id :remote-id :as mp}]
  (let [remote-peer (@attacker [:id remote-id])
        peer (@attacker [:id id])]

    (register-aes-key peer mp)

    {:reg-response (:public-key @remote-peer),
     :id id, :remote-id remote-id}))


(defn modify-msg
  "Modify encrypted message with MITM peer"
  [attacker {cipher :msg, id :id, remote-id :remote-id}]
  (let [peer (@attacker [:id id])
        remote-peer (@attacker [:id remote-id])
        malicious-plaintext (:msg @attacker)
        plaintext (aes/decrypt cipher (:aes-key @peer) :cbc)
        ciphertext (aes/encrypt malicious-plaintext (:aes-key @remote-peer) :cbc)]

    (swap! peer assoc :msg plaintext)
    (swap! remote-peer assoc :msg malicious-plaintext)

    {:msg ciphertext, :id id, :remote-id remote-id}))


(defn msg-listener
  "Listener sitting on the channel"
  [msg]
  (cond
    (:msg msg) (modify-msg attacker msg)
    (:reg-request msg) (modify-reg-request attacker msg)
    (:reg-response msg) (modify-reg-response attacker msg)
    :else (u/raise "Cannot decipher message")))


(defn get-attacker-peer-state
  "Obtain state variables of the attacker for a given peer"
  [id var]
  (get @(@attacker [:id id]) var))


(defn dh-negotiated-group
  "Start eavesdropping on channel and performing MITM"
  ([]
   (reset! attacker nil)
   (reset! channel msg-listener))
  ([key & args]
   (apply swap! attacker assoc key args)))
