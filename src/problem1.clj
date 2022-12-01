(ns problem1
  (:require [clojure.java.io :as io]))

;; (def input (->> "resources/input1-example.txt" io/reader line-seq))
(def input (->> "resources/input1.txt" io/reader line-seq))

(defn to-int [i] (when-not (= i "") (Integer/parseInt i)))
(def sum #(reduce + %))

(defn sum-of-calories [input]
  (->> input
       (map to-int)
       (partition-by nil?)
       (remove #(some nil? %))
       (map sum)
       (sort)
       reverse))

;; part 1 - max calories
(first (sum-of-calories input))

;; part 2
(->> input sum-of-calories (take 3) sum)

