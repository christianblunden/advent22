(ns problem5
  (:require [clojure.java.io :as io]))

;; (def input (->> "resources/input5-example.txt" io/reader line-seq (partition-by #(= "" %))))
(def input (->> "resources/input5.txt" io/reader line-seq (partition-by #(= "" %))))

(defn build-stacks [stacks]
  (let [[s & crates] (->> stacks
                          reverse
                          (map #(partition-all 4 %))
                          (map #(map second %)))]
    (reduce (fn [stacks row] (mapv #(if (= %2 \space) % (conj % %2)) stacks row)) 
            (mapv (constantly '()) s)
            crates)))

(defn process-commands [crane-capability stacks cmd]
  (let [[n from to] (map #(Integer/parseInt %) cmd)
        [moving staying] (split-at n (get stacks (dec from)))]
    (-> stacks
        (update (dec to) (fn [x] (reduce conj x (crane-capability moving))))
        (assoc (dec from) staying))))

(defn move-crane [[stacks _ cmds] crane]
  (->> cmds
       (map #(re-seq #"[0123456789]+" %))
       (reduce (partial process-commands crane) (build-stacks stacks))
       (map first)
       (apply str)))

(def crane-mover {:9000 identity :9001 reverse})
;;part1
(move-crane input (crane-mover :9000))
;;part2
(move-crane input (crane-mover :9001))