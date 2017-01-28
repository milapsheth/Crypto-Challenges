(ns set4.hmac-timing-attack
  (:require [util.tools :as u]
            [util.math :as math]))


(def HMAC-LEN 20)


(defn time-increase?
  "Check if time increase is in range of step"
  [previous-time run-time time-step]
  (let [epsilon (quot time-step 4)]
    (>= run-time
        (- (+ previous-time time-step) epsilon))))


(defn error-correct-runtime
  [verifier msg forged-signature i runtime]
  (if (< i (dec HMAC-LEN))
    (min (second (u/get-runtime verifier msg (assoc forged-signature (inc i) 1)))
         runtime)
    runtime))

(defn calculate-time-increase
  [verifier msg]
  (let [invalid-signature (vec (repeat HMAC-LEN 0))]
    (loop [guess 0
           best-guess 0
           max-time 0
           min-time 0xFFFFFFFF]
      (cond
        (< guess 256) (let [guessed-signature (assoc invalid-signature 0 guess)
                            [_ runtime] (u/get-runtime verifier msg guessed-signature)]
                        (recur (inc guess)
                               (if (> runtime max-time) guess best-guess)
                               (max max-time runtime)
                               (min min-time runtime)))
        :else (let [corrected-max-time (error-correct-runtime verifier msg (assoc invalid-signature 0 best-guess) 0 max-time)]
                ;; Corrected max time accounts for time increments if
                ;; more than 1 byte is correct since we're trying to
                ;; find one byte increment
                [min-time (- corrected-max-time min-time)])))))


(defn predict-next-byte
  [verifier msg known-bytes i previous-time time-step]
  (loop [guess 0
         [best-guess max-time] [guess 0]]
    (cond
      (< guess 256) (let [guessed-signature (assoc known-bytes i guess)
                          [result runtime] (u/get-runtime verifier msg guessed-signature)]
                      (cond
                        (time-increase? previous-time runtime time-step)
                        [guessed-signature
                         (error-correct-runtime verifier msg guessed-signature i runtime)]
                        :else (recur (inc guess) (max-key second [best-guess max-time] [guess runtime]))))
      :else (throw (Exception. (str "Could not predict byte " i " best guess: " best-guess " runtime: " max-time))))))


(defn hmac-timing-attack
  "Timing attack on HMAC"
  [verifier msg-to-forge]
  (let [[exec-time time-increment-per-byte]
        (calculate-time-increase verifier msg-to-forge)]
    (loop [i 0
           forged-signature (vec (repeat HMAC-LEN 0))
           execution-time exec-time]
      (cond
        (< i HMAC-LEN) (let [[better-forged-mac new-runtime]
                       (predict-next-byte verifier msg-to-forge forged-signature i execution-time time-increment-per-byte)]
                   (recur (inc i)
                          better-forged-mac
                          new-runtime))
        :else forged-signature))))


(defn hmac-timing-attack-lazy
  "Timing attack on HMAC evaluated lazily"
  [verifier msg-to-forge]
  (let [[exec-time time-increment-per-byte]
        (calculate-time-increase verifier msg-to-forge)]
    (defn lazy-evaluator [i forged-signature execution-time]
      (cond
        (< i HMAC-LEN) (let [[better-forged-mac new-runtime]
                             (predict-next-byte verifier msg-to-forge forged-signature i execution-time time-increment-per-byte)]
                         (lazy-seq (cons (better-forged-mac i)
                                         (lazy-evaluator (inc i)
                                                         better-forged-mac
                                                         new-runtime))))
        :else nil))
    (lazy-evaluator 0 (vec (repeat HMAC-LEN 0)) exec-time)))
