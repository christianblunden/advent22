(ns problem14
  (:require [clojure.core.matrix :as m]
            [clojure.string :as s]))

(m/set-current-implementation :ndarray)
(m/set-current-implementation :vectorz)
(def to-int #(Integer/parseInt %))
(defn parse-line [line] (->> line (re-seq #"\d+") (map to-int) (partition-all 2)))
(defn load-data [f] (->> f slurp s/split-lines (map parse-line)))
;; (def input (load-data "resources/input14-example.txt"))
(def input (load-data "resources/input14.txt"))

(def minmax (juxt #(apply min %) #(apply max %)))
(def start [0 500])

(defn to-range [from to base]
  (let [start (- (min from to) base)]
    (range start
           (+ start (abs (- from to)) 1))))

(defn mprint [m]
  (doseq [row (m/rows m)] 
   (println (str row)) ))

(defn draw-line [min-x grid [[from-x from-y] [to-x to-y]]]
  (reduce (fn [g' [row' col']]
            (m/mset g' row' col' "#"))
          grid
          (for [col (to-range from-x to-x min-x)
                row (to-range from-y to-y 0)]
            [row col])))

(defn draw-grid [input rows cols min-x]
  (->> input
       (map #(partition 2 1 %))
       (apply concat)
       (reduce (partial draw-line min-x) (m/fill (m/new-matrix rows cols) "."))))

(def neighbours [[1 0] [1 -1] [1 1]])

(defn sand-path [sand-start grid]
  (let [[rows cols] (m/shape grid)
        valid? (fn [[row col]] (and (<= 0 row (dec rows))
                                    (<= 0 col (dec cols))))
        free? (fn [[row col :as p]] (or (not (valid? p)) (= (m/mget grid row col) ".")))]
    (loop [curr sand-start]
      (if-let [p (->> neighbours (map #(m/add curr %)) (filter free?) first)]
        (when (valid? p)
          (recur p))
        curr))))

(defn total-sand [m]
  (as-> m x
      (m/emap (comp int first) x)
      (m/eq x 111)
      (m/esum x)))

;; part 1
(let [xs (->> input flatten (take-nth 2))
      [min-x max-x] (minmax xs)
      ys (->> input flatten (drop 1) (take-nth 2))
      [_ max-y] (minmax ys)
      sand-start (mapv - start [0 min-x])
      grid (draw-grid input (inc max-y) (inc (- max-x min-x)) min-x)]
  (->> (range)
       (reduce (fn [grid' _]
                 (if-let [[row col] (sand-path sand-start grid')]
                   (m/mset grid' row col "o")
                   (reduced grid')))
               grid)
       total-sand))

;; part 2
(let [ys (->> input flatten (drop 1) (take-nth 2))
      [_ max-y] (minmax ys)
      sand-start (mapv - start [0 0])
      input (concat input [[[0 (+ max-y 2)] [1000 (+ max-y 2)]]]) ; add floor
      grid (draw-grid input (+ max-y 3) 1001 0)]
  (->> (range)
       (reduce (fn [grid' _]
                 (let [[row col :as curr] (sand-path sand-start grid')]
                   (if (= curr sand-start)
                     (reduced (m/mset grid' row col "o"))
                     (m/mset grid' row col "o"))))
               grid)
       total-sand))