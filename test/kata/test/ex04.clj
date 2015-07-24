(ns kata.test.ex04
  (:require [kata.ex04 :refer :all]
            [clojure.test :refer :all]))

(deftest smallest-spread-in-temperature
  (is (= ["14" 61 59] (day-with-smallest-temperature-spread))))

(deftest smallest-spread-in-goals
  (is (= ["Aston_Villa" 46 47] (team-with-smallest-goals-spread))))
