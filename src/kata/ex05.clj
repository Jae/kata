(ns ^{:doc "http://codekata.com/kata/kata05-bloom-filters"}
  kata.ex05)

(defn md5-hashes
  [variants word]
  (->> (range 0 variants)
       (map (fn [variant]
              (-> (java.security.MessageDigest/getInstance "MD5")
                  (.digest (.getBytes (str word variant))))))))

(defprotocol Spellchecker
  (init [dictionary words])
  (check [dictionary word]))

(defrecord Dictionary [size hasher]
  Spellchecker
  (init [this words]
    (let [bits (java.util.BitSet. size)]
      (doseq [hash (->> words
                        (mapcat hasher)
                        (map #(.mod (java.math.BigInteger. 1 %)
                                    (java.math.BigInteger/valueOf size))))]
        (.set bits hash))
      (assoc this
        :hazher hasher
        :size size
        :bits bits)))
  (check [{bits :bits size :size} word]
    (->> (hasher word)
         (map #(.mod (java.math.BigInteger. 1 %)
                     (java.math.BigInteger/valueOf size)))
         (every? #(.get bits %)))))
