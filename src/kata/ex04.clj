(ns ^{:doc "http://codekata.com/kata/kata04-data-munging"}
  kata.ex04
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn with-smallest-difference-in-file
  [file-name field-extractor]
  (with-open [file (io/reader (io/resource file-name))]
    (let [lines (line-seq file)]
      (->> lines
           (map field-extractor)
           (remove nil?)
           (sort-by (fn [[id f1 f2]] (Math/abs (- f1 f2))))
           first))))

(defn day-with-smallest-temperature-spread
  []
  (with-smallest-difference-in-file
    "weather.dat"
    (fn [line]
      (let [columns (str/split (str/trim line) #"\s+")]
        (when (and (<= 3 (count columns))
                   (re-find #"\d+" (nth columns 1))
                   (re-find #"\d+" (nth columns 2)))
          [(nth columns 0)
           (read-string (re-find #"\d+" (nth columns 1)))
           (read-string (re-find #"\d+" (nth columns 2)))])))))

(defn team-with-smallest-goals-spread
  []
  (with-smallest-difference-in-file
    "football.dat"
    (fn [line]
      (let [columns (str/split (str/trim line) #"\s+")]
        (when (and (<= 9 (count columns))
                   (re-find #"\d+" (nth columns 6))
                   (re-find #"\d+" (nth columns 8)))
          [(nth columns 1)
           (read-string (re-find #"\d+" (nth columns 6)))
           (read-string (re-find #"\d+" (nth columns 8)))])))))
