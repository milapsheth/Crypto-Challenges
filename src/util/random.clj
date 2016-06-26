(ns util.random)


(def VALID-CHARS (mapv char (concat (range 48 58) ; 0-9
                                    (range 66 91) ; A-Z
                                    (range 97 123)))) ; a-z

(defn char
  "Returns a random char out of [A-Za-z0-9]"
  []
  (rand-nth VALID-CHARS))

(defn string
  "Returns a random string of given length"
  [length]
  (apply str (repeatedly length char)))

(defn byte-lst [n]
  (repeatedly n #(rand-int 256)))
