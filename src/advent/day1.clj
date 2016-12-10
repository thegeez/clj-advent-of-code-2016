(ns advent.day1
  (:require [clojure.spec :as s]
            [clojure.test :as test]
            [clojure.java.io :as io]))

(s/def ::step (s/cat :dir #{\R \L}
                     :dist (s/+ (set "0123456789"))))
(s/def ::steps (s/and string?
                      (s/conformer seq)
                      (s/* (s/cat :step ::step :sep (s/? (s/alt
                                                          :line-end #{\newline}
                                                          :comma-space
                                                          (s/cat :comma #{\,}
                                                                 :space #{\space})))))
                      (s/conformer
                       (fn [c]
                         (into []
                               (comp
                                (map :step)
                                (map (fn [s]
                                       (update s :dist (fn [d]
                                                         (Long/parseLong (apply str d))))))
                                (map (fn [s]
                                       (update s :dir {\R :right
                                                       \L :left}))))
                               c)))))

;;;/|\    north
;;; |  west   east
;;; |     south
;;; y x------------>
(def next-headings
  {:north {:left  :west
           :right :east}
   :east  {:left  :north
           :right :south}
   :south {:left  :east
           :right :west}
   :west  {:left  :south
           :right :north}})

(def xfactor {:north 0
              :east  1
              :south 0
              :west -1})

(def yfactor {:north  1
              :east   0
              :south -1
              :west   0})

(defn conform! [spec form]
  (let [c (s/conform spec form)]
    (if (= c ::s/invalid)
      (throw (ex-info "spec conform fail"
                      (s/explain-data spec form)))
      c)))

(defn dist [str-in]
  (let [steps (conform! ::steps str-in)
        travel (reduce
                (fn [acc step]
                  (let [{:keys [heading x y]} acc
                        {:keys [dir dist]} step
                        heading' (get-in next-headings [heading dir])
                        x' (+ x (* (xfactor heading') dist))
                        y' (+ y (* (yfactor heading') dist))]
                    {:heading heading'
                     :x x'
                     :y y'}))
                {:heading :north
                 :x 0
                 :y 0}
                steps)]
    (+ (Math/abs (:x travel))
       (Math/abs (:y travel)))))

(test/deftest tests
  (test/are [in out] (= (dist in) out)
    "R2, L3" 5
    "R2, R2, R2" 2
    "R5, L5, R5, R3" 12))

(defn day1 []
  (let [in (slurp (io/resource "day1.txt"))]
    (dist in)))

(comment
  (s/conform ::steps "R")
  (tests)
  (day1) ;; => 241
  )

(defn twice [str-in]
  (let [steps (conform! ::steps str-in)
        twice (reduce
               (fn [acc step]
                 (let [{:keys [heading x y seen]} acc
                       {:keys [dir dist]} step
                       heading' (get-in next-headings [heading dir])
                       xf (xfactor heading')
                       yf (yfactor heading')
                       x' (+ x (* xf dist))
                       y' (+ y (* yf dist))
                       more-seen (if (#{:west :east} heading')
                                   (for [x (range x x' xf)]
                                     [x y])
                                   (for [y (range y y' yf)]
                                     [x y]))]
                   (println "more-seen" more-seen
                            "dx" (range (inc (* xf dist)))
                            "dy" (range (inc (* yf dist))))
                   (if-let [twice (some seen more-seen)]
                     (reduced twice)
                     {:heading heading'
                      :x x'
                      :y y'
                      :seen (into seen more-seen)})))
               {:heading :north
                :x 0
                :y 0
                :seen #{}}
               steps)]
    twice))

(defn day1-1 []
  (let [in (slurp (io/resource "day1.txt"))]
    (twice in)))

(comment
  (twice "R8, R4, R4, R8") ; => [4 0]
  (day1-1) ;; => [11 -105] = 116
  )

(def first-dupe
  (fn [xf]
    (let [seen (volatile! #{})]
      (fn
        ([]
         (println "first-dupe init")
         (xf))
        ([r]
         (println "first-dupe complete" r)
         (xf r))
        ([r i]
         (println "first-dupe step" r i)
         (if (contains? @seen i)
           (reduced i)
           (do (vswap! seen conj i)
               (println "seen" @seen)
               (xf r i))))))))

(defn walk [xf]
  (fn
    ([]
     (println "walk init")
     (xf))
    ([r]
     (println "walk complete" r)
     (xf r))
    ([r i]
     (println "walk step" r i)
     (xf r i))))

(defn go []
  (println "Hello!")
  (transduce
   (comp
    walk
    (map (fn [i]
           (println "map" i)
           [i i ]))
    cat
    (filter (fn [i]
              (println "filter" i)
              (when (= i 9)
                i)))
    (take 1))
   (fn
     ([]
      (println "f init")
      )
     ([r]
      (println "f complete" r)
      r)
     ([r i]
      (println "f step" r i)
      i))
   {:heading :north
    :x 0
    :y 0}
   [1 2 3 4 5 6 7 8 9 0 1 2 3 4 5]
   )
  )
