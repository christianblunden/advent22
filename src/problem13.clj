(ns problem13
  (:require [clojure.string :as str]))

(defn load-data [f] (->> f slurp str/split-lines (remove str/blank?) (map read-string) (partition-all 2)))
;; (def input (load-data "resources/input13-example.txt"))
(def input (load-data "resources/input13.txt"))

(defn correct? [left right]
  (condp = [(vector? left) (vector? right)]
    [false false] (compare left right)
    [true false] (correct? left [right])
    [false true] (correct? [left] right)
    [true true] (if-let [val (some #{-1 1} (map correct? left right))]
                        val
                        (compare (count left) (count right)))))

;; part 1 "Elapsed time: 1.101529 msecs"
(time (->> input
           (map #(apply correct? %))
           (map-indexed #(vector (inc %) %2))
           (filter (comp neg? second))
           (map first)
           (apply +))) ; 6568

;; part 2 "Elapsed time: 5.563322 msecs"
(defn divider-index [signals divider]
  (->> signals
       (drop-while (fn [[_ packet]] (not= packet [[divider]])))
       ffirst
       inc))

(time (let [signals (->> input
                         (apply concat)
                         (into [[[2]] [[6]]])
                         (sort correct?)
                         (map-indexed vector))]
        (->> [2 6] (map #(divider-index signals %)) (apply *)))) ;; 19493