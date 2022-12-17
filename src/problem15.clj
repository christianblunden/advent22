(ns problem15)

(def to-int #(Integer/parseInt %))
(defn load-data [f] (->> f slurp (re-seq #"-?\d+") (map to-int) (partition-all 4)))
;; (def input (load-data "resources/input15-example.txt"))
(def input (load-data "resources/input15.txt"))

(defn md2 [x1 y1 x2 y2]
  (+ (abs (- x1 x2)) (abs (- y1 y2))))

(defn possible? [x y]
  (fn [[sx sy bx by]]
    (when (and (<= (md2 sx sy x y) (md2 sx sy bx by)) (not= [x y] [bx by]))
      1)))

(def xf1 (keep #(some (possible? % 10) input)))
(time (transduce xf1 + 0 (range -10 25)))

;part 1 "Elapsed time: 66431.345287 msecs"
(def xf2 (keep #(some (possible? % 2000000) input)))
(time (transduce xf2 + 0 (range -1000000 5000000))) ; 5142231


