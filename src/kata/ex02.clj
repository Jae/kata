(ns ^{:doc "http://codekata.com/kata/kata02-karate-chop"}
  kata.ex02)

(defn chop
  [needle haystack]
  (loop [from 0 to (count haystack)]
    (if-not (> (- to from) 1)
      (if (= (get haystack from) needle) from -1)
      (let [middle (int (/ (+ from to) 2))
            at-middle (get haystack middle)]
        (cond
         (= at-middle needle) middle
         (< at-middle needle) (recur (+ middle 1) to)
         (> at-middle needle) (recur from middle))))))
