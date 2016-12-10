(ns advent.day8
  (:require [clojure.string :as str]
            [clojure.spec :as s]
            [clojure.spec.gen :as gen]
            [clojure.java.io :as io]))

(def t1
  "rect 3x2
rotate column x=1 by 1
rotate row y=0 by 4
rotate column x=1 by 1
")

(def t1-screens
  [
"###....
###....
......."
"#.#....
###....
.#....."
"....#.#
###....
.#....."
".#..#.#
#.#....
.#....."
   ])

(def numbers "0123456789")
(def number (set numbers))
(def alphabet "abcdefghijklmnopqrstuvwxyz")
(def letter (set alphabet))

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

(s/def ::number (s/& (s/+ number)
                     (s/conformer
                      (fn [parsed]
                        (Long/parseLong (apply str parsed)))
                      (fn [out]
                        (seq (str out))))))

(s/def ::action (s/alt :rect (s/& (s/cat :text
                                         (match-seq "rect ")
                                         :wide ::number
                                         :x #{\x}
                                         :tall ::number)
                                  (s/conformer
                                   (fn [p]
                                     (select-keys p [:wide :tall]))))
                       :column (s/& (s/cat :text (match-seq "rotate column x=")
                                           :index ::number
                                           :by (match-seq " by ")
                                           :shift ::number)
                                    (s/conformer
                                     (fn [p]
                                       (select-keys p [:index :shift]))))
                       :row (s/& (s/cat :text (match-seq "rotate row y=")
                                        :index ::number
                                        :by (match-seq " by ")
                                        :shift ::number)
                                 (s/conformer
                                  (fn [p]
                                    (select-keys p [:index :shift]))))
                       ))
(s/def ::instructions (s/and string?
                             (s/conformer seq)
                             (s/* (s/cat
                                   :action ::action
                                   :newline #{\newline}))
                             (s/conformer
                              (fn [p]
                                (map :action p)))))

(defn parse [str-in]
  (let [actions (s/conform ::instructions str-in)]
    actions))

(defn str->screen [s]
  (->> s
       (str/split-lines)
       (mapv vec)))

(defn apply-action [screen action]
  (condp = (first action)
    :rect
    (let [{:keys [wide tall]} (second action)]
      (reduce
       (fn [screen coord]
         (println "screen " screen)
         (println "coord" coord)
         (assoc-in screen coord \#))
       screen
       (for [x (range wide)
             y (range tall)]
         [y x])))
    :column
    (let [{:keys [index shift]} (second action)]
      (vec
       (map-indexed
        (fn [row-idx row]
          (assoc row index
                 (get-in screen [(mod (- row-idx shift) (count screen)) index])))
        screen)))
    :row
    (let [{:keys [index shift]} (second action)]
      (update screen index
              (fn [row]
                (let [[pre post] (split-at (- (count row) shift) row)]
                  (-> []
                      (into post)
                      (into pre))))))))

(defn example []
  (let [actions (s/conform ::instructions t1)
        expected (map str->screen t1-screens)
        out (rest (reductions
              apply-action
              (vec (repeat 3 (vec (repeat 7 \.))))
              actions))]
    (doall (map
            (fn [o e]
              (println "= "(= o e))
              (println "o" o)
              (println "e" e))
            out expected))
    (= out expected)))

(defn day8 []
  (let [in (slurp (io/resource "day8.txt"))
        actions (s/conform ::instructions in)
        width 50
        height 6
        out (reduce
             apply-action
             (vec (repeat height (vec (repeat width \.))))
             actions)
        res (->> out
                 (map (comp count #(filter #{\#} %)))
                 (reduce +))]
    (doall (map println out))
    res))

(comment
  (day8) ;=> 123
  ;; code = AFBUPZBJPS
  (s/conform ::instructions t1)
  (s/explain ::instructions t1)
  (s/conform ::instructions "rect 33x12\n")
  (s/explain ::instructions "rect 33x12\n")
  (s/conform ::instructions "rotate column x=1 by 1\n")
  (s/explain ::instructions "rotate column x=1 by 1\n")


  )

