(ns problem7
  (:require [clojure.java.io :as io]
            [clojure.walk :as walk]))

;; (def input (->> "resources/input7-example.txt" io/reader line-seq))
(def input (->> "resources/input7.txt" io/reader line-seq))

(defn parse-line [line] 
  (condp re-matches line
    #"\$ cd (.+)" :>> (fn [[_ d]] {:cmd :cd :args d})
    #"\$ ls" :>> (fn [_] {:cmd :ls})
    #"dir (.+)" :>> (fn [[_ d]] {d {}})
    #"(\d+) (.+)" :>> (fn [[_ size name]] {name (Integer/parseInt size)})
    :unknown))

(defn cd [path args]
  (if (= args "..") 
    (-> path butlast vec)
    (conj path args)))

(defn ls [value listings]
  (into (or value {}) listings))

(def listing? (comp nil? :cmd))

(defn build-filesystem [input]
  (loop [tree {}
         path []
         [curr & rest] (map parse-line input)]
    (let [{:keys [cmd args]} curr]
      (if (nil? curr)
        tree
        (case cmd
          :cd (recur tree (cd path args) rest)
          :ls (recur (update-in tree path ls (take-while listing? rest)) 
                     path 
                     (drop-while listing? rest)))))))

(defn sum [dir]
  (transduce (filter number?) + (tree-seq map? vals dir)))

(defn total [fs]
  (loop [[node & rest] (into [] fs)
         dirs []]
    (if (nil? node)
      dirs
      (let [[k v] node]
        (if (map? v)
          (recur (concat (into [] v) rest) (conj dirs [k (sum v)]))
          (recur rest dirs))))))

;; part1
(->> input
     build-filesystem
     total
     (filter #(< (second %) 100000))
     (map second)
     (reduce +))

;; part 2
(let [totals (->> input build-filesystem total)
      root (get (into {} totals) "/")
      freespace (- 70000000 root)]
  (->> totals
       (sort-by second)
       (drop-while (fn [[_ v]] (< (+ freespace v) 30000000)))
       first
       second))