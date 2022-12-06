(ns problem6)

(defn find-start [message n]
  (->> message
       (partition-all n 1)
       (take-while #(not (apply distinct? %)))
       count
       (+ n)))

(time (find-start (slurp "resources/input6.txt") 4)) ;;part1 "Elapsed time: 2.332687 msecs"
(time (find-start (slurp "resources/input6.txt") 14)) ;;part2 "Elapsed time: 9.757325 msecs"