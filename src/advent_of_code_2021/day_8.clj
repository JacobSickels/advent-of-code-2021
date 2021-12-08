(ns advent-of-code-2021.day-8
  (:require [advent-of-code-2021.core :as core]
            [clojure.set :as set]
            [clojure.string :as str]))

(def data (core/read-file "resources/day_08.txt"))
(def test-data (core/read-file "resources/day_08_p.txt"))

(defn format-line [line]
  (let [[x y z] (partition-by #(= % "|") (str/split line #" | "))]
    [x z]))

(defn find-specific-digits-counts [[_ output]]
  (count (filter #(contains? #{2 3 4 7} (count %)) output)))

(defn part-1 [input]
    (reduce + (map #(find-specific-digits-counts (format-line %)) input)))

;; Please don't look a this too carefully
;; This code is trash
(defn find-digits [[line _]]
  (let [find-input (fn [l n] (first (filter #(= (count %) n) l)))
        ;; Initialize findings with character counts we already know
        findings {8 (find-input line 7)
                  7 (find-input line 3)
                  4 (find-input line 4)
                  1 (find-input line 2)}
        ;; combine 7 and 4, find which string is only 1 character away from combination (9)
        findings (assoc findings 9 (first (filter
                                            #(= (count (set/difference (set %)
                                                                       (set/union (set (find-input line 3)) 
                                                                                  (set (find-input line 4)))))
                                                1)
                                            (filter #(= (count %) 6) line))))
        ;; A five count number that is one different from 9
        findings (assoc findings 2 (first (filter #(and
                                                     (= (count (set/difference (set (get findings 9)) (set %))) 2)
                                                     (= (count %) 5)) 
                                                  line)))
        ;; A five count number that wasn't already found
        threes-or-5s (set (map set (filter #(and (not (contains? (set (vals findings)) %))  
                                                 (= (count %) 5)) line)))
        ;; A five count number that has all the characters from 1
        threes (first (filter #(empty? (set/difference (set (get findings 1)) %)) threes-or-5s))
        ;; A five count number that doesn't have all characters from 1
        fives (first (filter #(not (empty? (set/difference (set (get findings 1)) %))) threes-or-5s))
        ;; Add 3 to findings
        findings (assoc findings 3 (first (filter #(and
                                                     (= (set %) threes)
                                                     (= (count %) 5))
                                                  line)))
        ;; Add 5 to findings
        findings (assoc findings 5 (first (filter #(and
                                                     (= (set %) fives)
                                                     (= (count %) 5))
                                                  line)))
        ;; A six count number that wasn't already found
        zero-or-six (set (map set (filter #(and
                                             (not (contains? (set (map set (vals findings))) (set %)))
                                             (= (count %) 6))
                                          line)))
        ;; A six count number that has all the characters from 1
        zero (first (filter #(empty? (set/difference (set (get findings 1)) %)) zero-or-six))
        ;; A six count number that doesn't have all the characters from 1
        six (first (filter #(not (empty? (set/difference (set (get findings 1)) %))) zero-or-six))
        ;; Add 0 to findings
        findings (assoc findings 0 (first (filter #(and
                                                     (= (set %) zero)
                                                     (= (count %) 6))
                                                  line)))
        ;; Add 6 to findings
        findings (assoc findings 6 (first (filter #(and
                                                     (= (set %) six)
                                                     (= (count %) 6))
                                                  line)))]
    findings))
       
(defn find-number-from-string [output-str findings]
  (ffirst (filter #(= (set (val %)) (set output-str)) findings)))

(defn get-output-number [output findings]
    (map #(find-number-from-string % findings) (map set output)))

(defn get-output-for-line [input]
  (let [[line output] (format-line input)
        findings (find-digits [line output])]
    (get-output-number output findings)))

(defn part-2 [input]
  (reduce + (map #(Integer/parseInt (apply str %)) 
                 (map get-output-for-line input))))

