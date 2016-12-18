(ns advent.day18
  (:require [clojure.string :as str]))

(def example
  ".^^.^.^^^^
^^^...^..^
^.^^.^.^^.
..^^...^^^
.^^^^.^^.^
^^..^.^^..
^^^^..^^^.
^..^^^^.^^
.^^^..^.^^
^^.^^^..^^")

(defn get-t [in i]
  (if (<= 0 i (dec (count in)))
    (nth in i)
    \.))

(defn tile [l c r]
  (cond
    (and (= l c \^)
         (= r \.))
    \^
    (and (= c r \^)
         (= l \.))
    \^
    (and (= l \^)
         (= c r \.))
    \^
    (and (= r \^)
         (= l c \.))
    \^
    :else
    \.))

(defn next-row [in]
  (let [ins (vec in)
        out (for [i (range (count in))]
              (tile (get-t in (dec i))
                    (get-t in i)
                    (get-t in (inc i))))]
    (apply str out)
    ))

(let [rows (str/split-lines example)]
  (= (take (count rows) (iterate next-row (first rows)))
     rows)
  (->> (take (count rows) (iterate next-row (first rows)))
       (mapcat identity)
       (filter #{\.})
       count)
)

(defn day18 []
  (let [row "^^^^......^...^..^....^^^.^^^.^.^^^^^^..^...^^...^^^.^^....^..^^^.^.^^...^.^...^^.^^^.^^^^.^^.^..^.^"
        ;;c 40
        c 400000
        ]
    (->> (iterate next-row row)
         (take c)
         (mapcat identity)
         (filter #{\.})
         count)))

(comment
  (day18) ;; => 1978
  (day18) ;; => 20003246
  )
