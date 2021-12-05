(ns advent-of-code-2021.day-5
  (:require [advent-of-code-2021.core :as core]
            [clojure.string :as str]
            [clojure.set :as set]))

(def data 
  (map
    (fn [[p1 p2]] (vec [(str/split p1 #",") (str/split p2 #",")]))
    (map #(str/split % #" -> ") (core/read-file "resources/day_05.txt"))))

(def test-data (map
                 (fn [[p1 p2]] (vec [(str/split p1 #",") (str/split p2 #",")]))
                 (map #(str/split % #" -> ") ["0,9 -> 5,9"
                                              "8,0 -> 0,8"
                                              "9,4 -> 3,4"
                                              "2,2 -> 2,1"
                                              "7,0 -> 7,4"
                                              "6,4 -> 2,0"
                                              "0,9 -> 2,9"
                                              "3,4 -> 1,4"
                                              "0,0 -> 8,8"
                                              "5,5 -> 8,2"])))

(defn format-data [input]
  (map #(map (fn [[x y]] [(Integer/parseInt x) (Integer/parseInt y)]) %) input))

(defn points-between-no-diagonal [[x1 y1] [x2 y2]]
  (cond 
    (= x1 x2)
    (map #(vec [x1 %]) (apply core/inclusive-range (sort [y1 y2])))
    
    (= y1 y2)
    (map #(vec [% y1]) (apply core/inclusive-range (sort [x1 x2])))))

(defn part-1 [input]
  (->> (format-data input)
       (map (fn [[p1 p2]] (points-between-no-diagonal p1 p2)))
       (apply concat)
       (frequencies)
       (filter #(>= (second %) 2))
       (count)))


(defn get-diagonal-points [[x1 y1] [x2 y2]]
  (let [x-range (apply core/inclusive-range (sort [x1 x2]))
        x-range (if (= (first x-range) x1) x-range (reverse x-range))
        y-range (apply core/inclusive-range (sort [y1 y2]))
        y-range (if (= (first y-range) y1) y-range (reverse y-range))]
    (map vec (apply zipmap [x-range y-range]))))

(defn points-between-with-diagonal [[x1 y1] [x2 y2]]
  (cond
    (= x1 x2)
    (map #(vec [x1 %]) (apply core/inclusive-range (sort [y1 y2])))

    (= y1 y2)
    (map #(vec [% y1]) (apply core/inclusive-range (sort [x1 x2])))
    
    :else
    (get-diagonal-points [x1 y1] [x2 y2])))

(defn part-2 [input]
  (->> (format-data input)
       (map (fn [[p1 p2]] (points-between-with-diagonal p1 p2)))
       (apply concat)
       (frequencies)
       (filter #(>= (second %) 2))
       (count)))