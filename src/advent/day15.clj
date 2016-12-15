(ns advent.day15
  (:require [clojure.spec :as s]
            [clojure.test :as test]
            [clojure.java.io :as io]))

(def example-in
  "Disc #1 has 5 positions; at time=0, it is at position 4.
Disc #2 has 2 positions; at time=0, it is at position 1.
")

(defmacro match-seq [c]
  (let [ks (mapv keyword (repeatedly (count c) gensym))
        cat-kvs (mapcat vector ks (map hash-set c))]
    `(s/with-gen
       (s/&
        (s/cat ~@cat-kvs)
        (s/conformer
         (juxt ~@ks)
         (fn [out#]
           (zipmap ~ks out#))))
       (fn []
         (s/gen #{~c})))))

(def number (set "0123456789"))
(s/def ::number (s/& (s/+ number)
                     (s/conformer
                      #(Long/parseLong (apply str %)))))

(s/def ::disc (s/& (s/cat :_ (match-seq "Disc #")
                          :id ::number
                          :_ (match-seq " has ")
                          :pcount ::number
                          :_ (match-seq " positions; at time=0, it is at position ")
                          :p ::number
                          :_ #{\.})
                   (s/conformer #(dissoc % :_))))

(s/def ::discs (s/and string?
                      (s/conformer seq)
                      (s/* (s/& (s/cat :disc ::disc
                                       :newline #{\newline})
                                (s/conformer #(:disc %))))))

(defn day15 []
  (let [s (slurp (io/resource "day15.txt"))
        in (s/conform ::discs s)
        ;;in (s/conform ::discs example-in)
        in (conj in {:id 7 :pcount 11 :p 0})
        ]
    (some
     (fn [start-t]
       (let [state (map (fn [{:keys [id pcount p] :as disc}]
                          (let [out-pos (mod (+ (inc start-t) (dec id) p) pcount)]
                            out-pos)) in)]
         (when (every? zero? state)
           [start-t state])))
     (range))))
