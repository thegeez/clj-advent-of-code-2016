(ns advent.day5
  (:require [clojure.spec :as s]
            [clojure.test :as test]
            [clojure.java.io :as io]
            digest))

(digest/md5 "abc3231929")

(defn password []
  (let [in "cxdnnyjw"
        chars (transduce
               (comp
                (map #(str in %))
                (map digest/md5)
                (filter #(.startsWith % "00000"))
                (map #(nth % 5))
                (take 8))
               conj
               []
               (range))]
    (apply str chars)
    ))

(defn password2 []
  (let [in "cxdnnyjw"
        chars (transduce
               (comp
                (map #(str in %))
                (map digest/md5)
                (filter #(.startsWith % "00000"))
                (map (fn [x] [(Long/parseLong (str (nth x 5)) 16) (nth x 6)]))
                (filter (fn [[idx c]]
                          (<= 0 idx 7))))
               (fn
                 ([acc] acc)
                 ([acc [idx c]]
                    (println acc [idx c])
                    (if (nth acc idx)
                      acc
                      (let [acc (assoc acc idx c)]
                        (if (every? identity acc)
                          (reduced acc)
                          acc)))))
               (vec (repeat 8 nil))
               (range))]
    (apply str chars)
    ))

(comment
  (password);; "f77a0e6e"
  (password2) ;; "999828ec"
  )
