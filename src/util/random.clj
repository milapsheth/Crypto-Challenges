(ns util.random)


(def VALID-CHARS (mapv char (concat (range 48 58) ; 0-9
                                    (range 66 91) ; A-Z
                                    (range 97 123)))) ; a-z

(defn chr
  "Returns a random char out of [A-Za-z0-9]"
  []
  (rand-nth VALID-CHARS))

(defn string
  "Returns a random string of given length"
  [length]
  (apply str (repeatedly length chr)))

(defn byte-lst [n]
  (repeatedly n #(rand-int 256)))

(defn rand-long
  "Returns a random long value between 0 to n"
  [n]
  (long (rand n)))
