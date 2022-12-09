(ns problem9
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn parse-line [line] (update (str/split line #" ") 1 read-string))
(defn load-data [f] (->> f io/reader line-seq (map parse-line)))
;; (def input (load-data "resources/input9-example.txt"))
(def input (load-data "resources/input9.txt"))

(def cmds {"R" [1 0]
           "L" [-1 0]
           "U" [0 1]
           "D" [0 -1]})

(defn process-command2 [[head & rest] cmd]
  (reduce (fn [heads tail]
            (let [head (last heads)
                  diff (mapv - head tail)]
              (conj heads (if (some #{-2 2} diff)
                            (mapv + tail (map #({-2 -1 2 1} % %) diff))
                            tail))))
          [(mapv + head (cmds cmd))]
          rest))

(defn knots [n]
  (->> input
     (map (fn [[cmd n]] (repeat n cmd)))
     flatten
     (reductions process-command2 (repeat n [0 0]))
     (map last)
     distinct
     count))

(knots 2)
(knots 10)