(ns advent.day6
  (:require [clojure.spec :as s]
            [clojure.test :as test]
            [clojure.java.io :as io]
            [clojure.string :as str]))

(def t1
  "eedadn
drvtee
eandsr
raavrd
atevrs
tsrnev
sdttsa
rasrtv
nssdts
ntnada
svetve
tesnvt
vntsnd
vrdear
dvrsen
enarar")

(defn decode [in-str]
  (->> in-str
       str/split-lines
       (apply map (fn [& column]
                    (println column)
                    (->> column
                         frequencies
                         (apply max-key second)
                         first)))
       (apply str)))

(defn decode2 [in-str]
  (->> in-str
       str/split-lines
       (apply map (fn [& column]
                    (println column)
                    (->> column
                         frequencies
                         (apply min-key second)
                         first)))
       (apply str)))

(comment
  (decode t1) ;;"easter"
  (decode (slurp (io/resource "day6.txt"))) ;; "xdkzukcf"
  (decode2 t1);;"advent"
  (decode2 (slurp (io/resource "day6.txt"))) ;; "cevsgyvd"

  )
