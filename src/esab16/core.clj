(ns esab16.core
  (:import (java.nio ByteBuffer)))

(def ^{:private true} c
  (char-array (str "\u200B\u200C\u200D\uFEFF\u202C\u2060\u2061\u2062"
                   "\u2063\u2064\u206A\u206B\u206C\u206D\u206E\u206F")))
(def ^{:private true} x (zipmap c (range 0 16)))

(defn- enc-char [n]
  (get c n))

(defn- dec-char [char]
  (get x char 0))

(defn encode [data]
  (let [length (count data)
        buf (StringBuffer. (* length 2))]
    (doseq [i (range 0 length)]
      (.append buf (-> (get data i)
                       (bit-and 0xF0)
                       (bit-shift-right 4)
                       enc-char))
      (.append buf (-> (get data i)
                       (bit-and 0x0F)
                       enc-char)))
    (.toString buf)))

(defn decode [string]
  (let [length (if-not (even? (count string))
                 (- (count string) 1)
                 (count string))
        buf (ByteBuffer/allocate (/ length 2))]
    (doseq [i (range 0 length 2)]
      (.put buf (unchecked-byte (bit-or (-> (.charAt string i)
                                            dec-char
                                            (bit-shift-left 4))
                                        (-> (.charAt string (inc i))
                                            dec-char)))))
    (.array buf)))

(defn str-encode
  ([string] (str-encode string "UTF-8"))
  ([string charset]
   (encode (.getBytes string charset))))

(defn str-decode
  ([string] (str-decode string "UTF-8"))
  ([string charset]
   (String. (decode string) charset)))
