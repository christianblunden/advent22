(ns problem10
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn parse-line [line] (let [s (str/split line #" ")]
                          (if (= 1 (count s)) s (update s 1 read-string))))
(defn load-data [f] (->> f io/reader line-seq (map parse-line)))
;; (def input (load-data "resources/input10-example.txt"))
(def input (load-data "resources/input10.txt"))

(defn cycles [input]
  (reduce (fn [result [cmd reg]]
            (let [{:keys [cycle x] :as prev} (last result)]
              (cond-> result
                true           (conj (update prev :cycle inc))
                (= cmd "addx") (conj {:cycle (+ cycle 2) :x (+ x reg)}))))
          [{:cycle 1 :x 1}]
          input))

; part1 "Elapsed time: 0.997391 msecs"
(time (->> [19 59 99 139 179 219]
           (map (partial nth (cycles input)))
           (map #(apply * (vals %)))
           (reduce +)))

(defn crt [{:keys [cycle x]}]
  (if (<= (dec x) (mod (dec cycle) 40) (inc x)) "#" "."))

;; part2 "Elapsed time: 0.941747 msecs"
(time (->> input
           cycles
           (map crt)
           (partition-all 40)
           (map #(apply str %))))