(ns kata.test.ex05
  (:require [kata.ex05 :refer :all]
            [clojure.test :refer :all]))

(defn check-spelling
  [words word]
  (let [hasher (partial md5-hashes 5)
        dict (init (map->Dictionary {:size (* 10 1024 1024) :hasher hasher})
                   words)]
    (check dict word)))

(deftest bloom-filter-by-md5-hashes
  (is (= true (check-spelling ["ABC"] "ABC")))
  (is (= false (check-spelling ["ABC"] "ABE"))))
