(ns advent.day3
  (:require [clojure.spec :as s]
            [clojure.test :as test]
            [clojure.java.io :as io]))

(s/def ::line (s/+ (s/cat :whitespace (s/+ #{\space})
                          :num (s/+ (set "0123456789")))))

(s/def ::nums (s/and string?
                     (s/conformer seq)
                     (s/+ (s/cat :line ::line
                                 :newline #{\newline}))
                     (s/conformer
                      (fn [res]
                        (mapv (fn [line]
                                (println "line" line)
                                (mapv (comp #(Long/parseLong %) #(apply str %) :num) (:line line)))
                              res)))))

(defn is-possible [[a b c]]
  (and (> (+ a b) c)
       (> (+ a c) b)
       (> (+ b c) a)))

(defn day3 []
  (let [in (slurp (io/resource "day3.txt"))
        nums (s/conform ::nums in)
        res (->> nums
                 (filter is-possible)
                 count)]
    res))

(defn transpose3 [c]
  (->> c
       (partition 3)
       (mapcat #(apply map vector %))))

(defn day3-1 []
  (let [in (slurp (io/resource "day3.txt"))
        nums (s/conform ::nums in)
        nums (transpose3 nums)
        res (->> nums
                 (filter is-possible)
                 count)]
    res))

(comment
  (s/explain-data (s/and string?
                         (s/conformer seq)
                         (s/+ (s/cat :whitespace (s/+ #{\space})
                                     :num (s/+ (set "0123456789"))))) "   12")
  (s/explain (s/+ #{\space}) (seq " "))
  (day3) ; => 869
  (day3-1) ; 1544
  )
