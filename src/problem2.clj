(ns problem2
  (:require [clojure.java.io :as io]))

(def input (->> "resources/input2-example.txt" io/reader line-seq))
;; (def input (->> "resources/input2.txt" io/reader line-seq))