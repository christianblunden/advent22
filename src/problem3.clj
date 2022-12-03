(ns problem3
  (:require [clojure.java.io :as io]
            [clojure.set :as set]))
;; (def input (->> "resources/input3-example.txt" io/reader line-seq))
(def input (->> "resources/input3.txt" io/reader line-seq))
(def scores (->> (seq "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ")
                 (map-indexed (fn [i c] [(char c) (inc i)]))
                 (into {})))
;;part1
(defn duplicates [line]
  (let [compartments (split-at (/ (count line) 2) line)]
    (apply clojure.set/intersection (map set compartments))))
(->> input
     (map (comp scores first duplicates))
     (reduce +))
;;part2
(defn badges [group]
  (apply clojure.set/intersection (map set group)))
(->> input
     (partition-all 3)
     (map (comp scores first badges))
     (reduce +))