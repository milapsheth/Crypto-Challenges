(ns util.padding)


;; Padding scheme

;; PKCS#7
(defn- pkcs7-padding
  "PKCS7 padding to the plaintext"
  [plaintext block-size]
  (let [len (count (vec plaintext))
        bytes-to-pad (- block-size (rem len block-size))]
    (concat plaintext (repeat bytes-to-pad bytes-to-pad))))

(defn- pkcs7-remove-padding
  "PKCS#7 remove padding from text"
  [padded-text block-size]
  (let [num-padded-bytes (last padded-text)]
    (drop-last num-padded-bytes padded-text)))

(defn- validate-pkcs7
  "Validate PKCS#7 padding"
  [text block-size]
  (let [padded-elem (last text)]
    (and (> padded-elem 0)
         (<= padded-elem block-size)
         (every? #(= % padded-elem) (take-last padded-elem text)))))


;; Padding function
(defn pad-plaintext
  "Pad bytes to plaintext using given mode"
  [plaintext block-size mode]
  (({:pkcs7 pkcs7-padding} mode)
   plaintext block-size))


(defn validate-padding
  "Validate padded text under given mode"
  [padded-text block-size mode]
  (({:pkcs7 validate-pkcs7} mode)
   padded-text block-size))


(defn unpad-plaintext
  "Remove padded bytes from decrypted plaintext"
  [padded-text block-size mode]
  (when-not (validate-padding padded-text block-size :pkcs7)
    (throw (Exception. "Invalid padding")))
  
  (({:pkcs7 pkcs7-remove-padding} mode)
   padded-text block-size))


