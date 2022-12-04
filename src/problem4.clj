(ns problem4
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.set :as set]))

(defn parse-sections [sections]
  (->> (str/split sections #"-")
       (map read-string)
       ((fn [[start end]] (set (range start (inc end)))))))

(defn parse-line [line]
  (map parse-sections (str/split line #",")))

;; (def input (->> "resources/input4-example.txt" io/reader line-seq (map parse-line)))
(def input (->> "resources/input4.txt" io/reader line-seq (map parse-line)))

;;part 1
(defn fully-contains [[l r]]
  (let [overlap (clojure.set/intersection l r)]
    (or (= overlap l) (= overlap r))))
(count (filter fully-contains input))

;;part 2
(defn partial-contains [[l r]]
  (< 0 (count (clojure.set/intersection l r))))
(count (filter partial-contains input))