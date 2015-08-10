(ns ^{:doc "http://codekata.com/kata/kata06-anagrams"}
  kata.ex06)

(defn anagrams
  [words]
  (->> words
       distinct
       (group-by #(apply str (sort %)))
       vals
       (filter #(> (count %) 1))))
