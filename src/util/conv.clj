(ns util.conv)


(defn hex-to-int
  [ch]
  (let [c (int ch)]
    (if (< c 97)
      (- c 48)
      (- c 87))))

(defn int-to-hex
  [c]
  (char (+ c (if (< c 10) 48 87))))

(defn hexstr-to-str
  [data]
  (map #(char (+ (bit-shift-left (hex-to-int (first %)) 4) (hex-to-int (second %))))
       (partition 2 2 0 (seq data))))

(defn str-to-lst
  [data]
  (map hex-to-int (seq data)))

(defn lst-to-str [data] (clojure.string/join (map int-to-hex data)))

(defn reduce'
  [f b l & coll]
  (if (empty? l)
    b
    (loop [lst (apply map vector (cons l coll))
           acc b]
      (if (empty? lst)
        acc
        (recur (rest lst) (apply f acc (first lst)))))))
