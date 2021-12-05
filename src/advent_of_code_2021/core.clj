(ns advent-of-code-2021.core
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io])
  (:import (java.io PushbackReader)))

(defn read-file [file]
  (with-open [reader (clojure.java.io/reader file)]
    (reduce conj [] (line-seq reader))))

(defn load-edn
  [resource-file]
  (edn/read (PushbackReader. (io/reader (io/resource resource-file)))))

(defn inclusive-range [start end]
  (range start (inc end)))

