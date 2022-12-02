(ns problem2
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.set :as set]))

;; (def input (->> "resources/input2-example.txt" io/reader line-seq (map #(str/split % #" "))))
(def input (->> "resources/input2.txt" io/reader line-seq (map #(str/split % #" "))))

(def rps {"A" :rock
          "B" :paper
          "C" :scissors})

(def winners {:scissors :rock
              :paper :scissors
              :rock :paper})

(def scores {:rock 1
             :paper 2
             :scissors 3
             :win 6
             :draw 3
             :loss 0})

(defn score [[opponent me]]
  (let [outcome (if (= opponent me) :draw
                    (if (= me (winners opponent)) :win :loss))]
    (+ (scores me) (scores outcome))))

;; part 1
(defn part1 [line] (map (merge rps {"X" :rock
                                    "Y" :paper
                                    "Z" :scissors}) line))

(->> input
     (map (comp score part1))
     (reduce +))

;; part 2
(def strategies {"X" (set/map-invert winners)
                 "Y" identity
                 "Z" winners})

(defn part2 [[o s]]
  (let [opponent (rps o)]
    [opponent ((strategies s) opponent)]))

(->> input
     (map (comp score part2))
     (reduce +))
