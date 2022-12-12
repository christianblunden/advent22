(ns problem12
  (:require [clojure.core.matrix :as m]
            [clojure.string :as s]))

(m/set-current-implementation :ndarray)

(defn parse-line [line] (->> line (re-seq #"\w") (map (comp int first))))
(defn load-data [f] (->> f slurp s/split-lines (map parse-line) m/matrix))
;; (def input (load-data "resources/input12-example.txt"))
(def input (load-data "resources/input12.txt"))

(def ^:private inf (Long/MAX_VALUE))

(defn neighbours
  "Returns n's neighbors, filtered by unvisited"
  ([g n] (get g n {}))
  ([g n uv] (select-keys (neighbours g n) uv)))

(defn update-costs
  [g costs curr unvisited]
  (let [curr-cost (costs curr)]
    (reduce
     (fn [c [nbr _]] (update-in c [nbr] #(min (inc curr-cost) %))) ; cost-fn: cost of moving to neighbour is +1. update cost of neighbour to min of it's existing cost or current-cost+1
     costs
     (neighbours g curr unvisited))))

(defn dijkstra
  [g src & {:keys [target]}]
  (loop [costs (assoc (zipmap (keys g) (repeat inf)) src 0) ;; set all initial costs to infinity
         curr src
         unvisited (disj (apply hash-set (keys g)) src)]
    (if (or (empty? unvisited) (= inf (costs curr)))
      (if (= target curr)
        (costs target)
        costs)
      (let [costs' (update-costs g costs curr unvisited)
            curr' (first (sort-by costs' unvisited))]
        (if (= target curr)
          (costs' target)
          (recur costs'
                 curr'
                 (disj unvisited curr')))))))

(defn bounded? [[h w]] (fn [[r c]] (and (<= 0 r h) (<= 0 c w))))

(defn valid-neighbours [matrix [cy cx :as curr] size]
  (into {}
        (comp
         (map (fn [i] (m/add curr i)))
         (filter (bounded? size))
         (map (juxt identity (fn [[y x]] (m/mget matrix y x))))
         (filter (fn [[_ val]] (<= val (inc (m/mget matrix cy cx))))))
        [[0 1] [1 0] [-1 0] [0 -1]]))

(defn to-graph [matrix size]
  (->> matrix
       m/index-seq
       (reduce #(assoc % %2 (valid-neighbours matrix %2 size)) {})))

(defn find [m x]
  (->> m
       m/index-seq
       (some #(when (= x (apply (partial m/mget input) %)) %))))

(defn solve [input]
  (let [size (mapv dec (m/shape input))
        [start-y start-x :as start] (find input (int \S))
        [target-y target-x :as target] (find input (int \E))
        input (-> input (m/mset start-y start-x (int \a)) (m/mset target-y target-x (int \z)))
        graph (to-graph input size)]
    (dijkstra graph start :target target)))

;; part 1
(time (solve input)) ; 361  "Elapsed time: 7367.573227 msecs"

;; part 2
(defn find-all [m x]
  (->> m
       m/index-seq
       (filter #(when (= x (apply (partial m/mget input) %)) %))))

(defn solve2 [input]
  (let [size (mapv dec (m/shape input))
        [start-y start-x] (find input (int \S))
        [target-y target-x :as target] (find input (int \E))
        input (-> input (m/mset start-y start-x (int \a)) (m/mset target-y target-x (int \z)))
        graph (to-graph input size)]
    (->> (find-all input (int \a)) 
         (transduce
          (comp
           (map #(dijkstra graph % :target target))
           (remove map?))
          conj)
         sort
         first)))

(time (solve2 input)); 354 "Elapsed time: 435772.804905 msecs"