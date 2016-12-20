(ns advent.day20
  (:require [clojure.spec :as s]
            [clojure.java.io :as io]))

(def max-ip 4294967295)


(def number (set "0123456789"))
(s/def ::number (s/& (s/+ number)
                     (s/conformer
                      #(Long/parseLong (apply str %)))))

(s/def ::ranges (s/and string?
                       (s/conformer seq)
                       (s/*
                        (s/& (s/cat :low ::number
                                    :dash #{\-}
                                    :high ::number
                                    :newline #{\newline})
                             (s/conformer
                              #(select-keys % [:low :high]))))))

(defn day20 []
  (let [in (slurp (io/resource "day20.txt"))
        #_in
        #_"5-8
0-2
4-7
"
        ranges (s/conform ::ranges in)
        markers (reduce
                 (fn [flags {:keys [low high]}]
                   (assoc flags low :begin high :end))
                 (sorted-map)
                 ranges)]
    (reduce
     (fn [[ip in-range-count] [mip marker]]
       (cond
         (and (zero? in-range-count)
              (< ip mip))
         (reduced ip)
         (= marker :begin)
         [mip (inc in-range-count)]
         (= marker :end)
         [(inc mip) (dec in-range-count)]
         :else
         (throw (ex-info "odd range" {:ip ip :in-range-count in-range-count
                                      :mip mip :marker marker}))))
     [0 0]
     markers)
    ))

(comment
  (s/conform ::ranges (slurp (io/resource "day20.txt")))
  (day20) ;;=> 32259706
  )


(defn day20-2 []
  (let [in (slurp (io/resource "day20.txt"))
        ;;max-ip 9
        #_in
        #_"5-8
0-2
4-7
"

        ranges (s/conform ::ranges in)
        markers (reduce
                 (fn [flags {:keys [low high]}]
                   (assoc flags low :begin high :end))
                 (sorted-map)
                 ranges)]
    (first
     (reduce
      (fn [[allowed-count range-start range-count] [mip marker]]
        (cond
          (and (= marker :end)
               (= range-count 1))
          [(- allowed-count (- (inc mip) range-start)) nil 0]
          (= marker :end)
          [allowed-count range-start (dec range-count)]
          (and (= marker :begin)
               (= range-count 0))
          [allowed-count mip 1]
          (= marker :begin)
          [allowed-count range-start (inc range-count)]))
      [(inc max-ip) nil 0]
      markers))
    ))
(comment
  (day20-2) ;;=> 113
  )








