(ns problem11
  (:require [clojure.java.io :as io]))

(defn parse-group [[monkey items op test truthy falsy]]
  {:index (Integer/parseInt (re-find #"\d+" monkey))
   :items (->> items (re-seq #"\d+") (mapv (comp biginteger read-string)))
   :on-inspection (->> op (re-find #"([*+]) (old|\d+)") rest (map read-string))
   :test (Integer/parseInt (re-find #"\d+" test))
   :throws-to {true (Integer/parseInt (re-find #"\d+" truthy))
            false (Integer/parseInt (re-find #"\d+" falsy))}
   :count 0})
(defn load-data [f] (->> f io/reader line-seq (partition-all 7) (mapv parse-group)))
;; (def input (load-data "resources/input11-example.txt"))
(def input (load-data "resources/input11.txt"))

(defn process-items [stress-fn divisor {[f val] :on-inspection, test :test, throws-to :throws-to}]
  (comp (map #(stress-fn ((resolve f) % (if (number? val) val %)) divisor))
        (map #(vector (throws-to (zero? (mod % test))) %))))

(defn round2 [process-fn troop]
  (reduce (fn [new-troop {:keys [index] :as m}]
            (transduce (process-fn m)
                       (completing
                        (fn [agg [new-index item]]
                          (-> agg
                              (update-in [index :count] inc)
                              (update-in [index :items] (comp vec rest))
                              (update-in [new-index :items] conj item))))
                       new-troop
                       (get-in new-troop [index :items])))
          troop
          troop))

(defn monkey-play [n xf input]
  (->> (range n)
       (reduce (fn [rnd _] (round2 xf rnd)) input)
       (map :count)
       (sort >)
       (take 2)
       (apply *)))

;; part 1 "Elapsed time: 2.369649 msecs"
(time (monkey-play 20 #(process-items quot 3 %) input))

;; part 2 "Elapsed time: 873.787519 msecs"
(time (monkey-play 10000 #(process-items mod (transduce (map :test) * input) %) input))