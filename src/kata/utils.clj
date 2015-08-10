(ns kata.utils
  (:require [clojure.java.io :as io]))

(defn file-lines
  [file-name]
  (with-open [file (io/reader (io/resource file-name))]
    (doall (line-seq file))))
