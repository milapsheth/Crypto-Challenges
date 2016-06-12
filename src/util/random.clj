(ns util.random)


(def VALID-CHARS (mapv char (concat (range 48 58) ; 0-9
                                    (range 66 91) ; A-Z
                                    (range 97 123)))) ; a-z

(defn random-char
  "Returns a random char out of [A-Za-z0-9]"
  []
  (rand-nth VALID-CHARS))

(defn random-string
  "Returns a random string of given length"
  [length]
  (apply str (repeatedly length random-char)))
