(ns kata.test.ex06
  (:require [kata.ex06 :refer :all]
            [clojure.test :refer :all]
            [kata.utils :as u]))

(deftest find-longest-words-that-are-anagrams
  (let [longest-words (->> (anagrams (u/file-lines "wordlist.txt"))
                           (sort-by (comp count first) >)
                           first)]
    (is (= ["acoustoelectrically" "electroacoustically"] longest-words))))

(deftest find-largest-set-of-anagrams
  (let [longest-words (->> (anagrams (u/file-lines "wordlist.txt"))
                           (sort-by count >)
                           first)]
    (is (= ["alerts" "alters" "artels" "estral" "laster" "rastle" "ratels" "salter" "slater" "staler" "stelar" "talers" "tarsel"] longest-words))))
