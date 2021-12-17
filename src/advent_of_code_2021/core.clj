(ns advent-of-code-2021.core
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.math.combinatorics :as combo])
  (:import (java.io PushbackReader)))

(defn read-file [file]
  (with-open [reader (clojure.java.io/reader file)]
    (reduce conj [] (line-seq reader))))

(defn load-edn
  [resource-file]
  (edn/read (PushbackReader. (io/reader (io/resource resource-file)))))

(defn inclusive-range [start end]
  (range start (inc end)))

(defn points-around [point]
  (remove (fn [p] (= point p))
          (apply combo/cartesian-product
                 (map #(range (dec %) (+ 2 %)) point))))

(defn point-grid 
  ([width height inclusive?]
   (let [w-range        ((if inclusive? inclusive-range range) 0 width)
         h-range        ((if inclusive? inclusive-range range) 0 height)]
     (combo/cartesian-product w-range h-range)))
  ([min-x max-x min-y max-y]
   (let [w-range        (inclusive-range min-x max-x)
         h-range        (inclusive-range min-y max-y)]
     (combo/cartesian-product w-range h-range))))
   
