(ns advent.day24
  (:require [clojure.string :as str]
            [clojure.java.io :as io]))


(def example-in
  "###########
#0.1.....2#
#.#######.#
#4.......3#
###########")

(defn parse [in-str]
  (->> in-str
       str/split-lines
       (mapv (fn [row]
               (mapv (fn [i]
                       (if-let [num (get (zipmap "0123456789" [0 1 2 3 4 5 6 7 8 9]) i)]
                         num
                         i)) row)))))

(def ats (atom []))
(defn bfs [start succ stop]
  (if (stop start)
    [0 start]
    (loop [doing (-> (sorted-set-by
                      (fn [l r]
                        (let [stepsc (compare (:steps l) (:steps r))]
                          (if (zero? stepsc)
                            (let [atc (compare (:at l) (:at r))]
                              (if (zero? atc)
                                (let [seenc (compare (vec (:seen l)) (vec (:seen r)))]
                                  seenc)
                                atc))
                            stepsc))))
                     (conj start))
           ;; {[at seen] steps}
           visited {}]
      (println " doing " (count doing))
      (if (empty? doing)
        :undoable
        (let [best (first doing)
              _ (println "best" best)
              #_ (when (< 30 (:steps best))
                   (throw (ex-info "waayyyy too far" {})))
              _ (swap! ats conj best)
              stopper (stop best)]
          (if (= true stopper)
            best
            (let [orig-best best
                  best (if (and (= :stage-two stopper)
                                (not (:stage-two best)))
                         (-> best
                             (update :seen disj 0)
                             (assoc :stage-two true)
                             (assoc :best-stage-one best))
                         best)
                  _ (println "best2" best)
                  next (succ best)
                  [next visited] (reduce
                                  (fn [[next visited] n]
                                    (let [vk [(:at n) (:seen n) (:stage-two n)]
                                          best-steps (get visited vk)]
                                      (if (or (not best-steps)
                                              (< (:steps n) best-steps))
                                        [(conj next n)
                                         (assoc visited vk (:steps n))]
                                        [next
                                         visited])))
                                  [[] visited]
                                  next)]
              (recur (-> doing
                         (disj orig-best)
                         (into next))
                     visited))))))))

(defn walk [world state dir]
  ;;(println "world" world)
  ;;(println "state" state)
  ;;(println "dir" dir)
  (let [at (:at state)
        [to tile steps] (loop [[x y] at
                               steps 0]
                          (let [tile (get-in world [y x] \#)
                                y' (cond
                                     (= dir :up)
                                     (dec y)
                                     (= dir :down)
                                     (inc y)
                                     :else y)
                                x' (cond
                                     (= dir :left)
                                     (dec x)
                                     (= dir :right)
                                     (inc x)
                                     :else x)
                                tile (get-in world [y' x'] \#)]
                            
                            ;;(println "[x y]" [x y] dir "[x' y']" [x' y'] "tile" tile)
                            (cond
                              (contains? #{0 1 2 3 4 5 6 7 8 9} tile)
                              [[x' y'] tile (inc steps)]
                              (let [tile-ahead (get-in world [y' x'] \#)]
                                (= tile-ahead \#))
                              [[x y] \# steps]
                              :else ;; ahead is \., check if this is an intersection
                              (let [intersection (let [around {:up [[-1 -1] [1 -1]]
                                                               :right [[1 -1] [1 1]]
                                                               :down [[-1 1] [1 1]]
                                                               :left [[-1 -1] [-1 1]]}
                                                       sides (for [[dx dy] (get around dir)
                                                                   :let [x'' (+ x dx)
                                                                         y'' (+ y dy)
                                                                         side (get-in world [y'' x''] \#)
                                                                         ;;_ (println at [x'' y''] side dir)
                                                                         ]
                                                                   :when (= \. side)]
                                                               [x'' y''])]
                                                   #_(when (= [x y] [139 23])
                                                     (println "view" sides "dir" dir
                                                              "dd" (get around dir)
                                                              "sides"
                                                              (for [[dx dy] (get around dir)
                                                                    :let [x'' (+ x dx)
                                                                          y'' (+ y dy)
                                                                          side (get-in world [y'' x''] \#)
                                                                          ;;_ (println at [x'' y''] side dir)
                                                                          ]
                                                                    :when (= \. side)
                                                                    ]
                                                                [x'' y'' side])))
                                                   (seq sides))]
                                (when (or
                                       (= [x y] [1 1])
                                       (= [x y] [139 23]))
                                  (println "intersection" intersection)
                                  )
                                (if intersection
                                  [[x' y'] tile (inc steps)]
                                  (recur [x' y']
                                         (inc steps))
                                  )))))]
    ;;(println "moves" [to tile steps])
    (when-not (zero? steps)
      (let [seen (cond
                   (= tile \#)
                   (:seen state)
                   (= tile \.)
                   (:seen state)
                   :else
                   (conj (:seen state) tile))]
        (assoc state
               :at to
               :seen seen
               :steps (+ (:steps state) steps))))))

(defn day24 [str-in]
  (let [world (parse str-in)
        all-stops (set (filter #{0 1 2 3 4 5 6 7 8 9}
                               (mapcat identity world)))
        start {:steps 0
               :seen #{0}
               :at (some
                    (fn [y]
                      (when-let [x (some
                                    (fn [[x c]]
                                      (when (= c 0)
                                        x))
                                    (map list (range) (get world y)))]
                        [x y]))
                    (range (count world)))}
        stop (fn [state]
               (= all-stops (:seen state)))
        succ (fn [state]
               (for [dir [:up :right :down :left]
                     :let [next (walk world state dir)]
                     :when next]
                 (do
                   ;;(println "next" next "dir" dir)
                   next)))]
    (bfs start succ stop)))

(defn showx [str-in ats]
  (let [world (parse str-in)
        xs (into #{} (map :at ats))]
    ;;(println "xs" xs)
    (println "-----------")
    (doseq [y (range (count world))]
      (let [row (get world y)]
        (println (apply str
                        (map-indexed
                         (fn [i x]
                           (cond
                             (contains? #{0 1 2 3 4 5 6 7 8 9} x)
                             x
                             (contains? xs [i y])
                             "X"
                             :else
                             x))
                         row)))))))

(defn day24-2 [str-in]
  (let [world (parse str-in)
        all-stops (set (filter #{0 1 2 3 4 5 6 7 8 9}
                               (mapcat identity world)))
        start {:steps 0
               :seen #{0}
               :at (some
                    (fn [y]
                      (when-let [x (some
                                    (fn [[x c]]
                                      (when (= c 0)
                                        x))
                                    (map list (range) (get world y)))]
                        [x y]))
                    (range (count world)))}
        stop (fn [state]
               (cond
                 (not (:stage-two state))
                 (if (= all-stops (:seen state))
                   :stage-two
                   false)
                 (:stage-two state)
                 (= all-stops (:seen state))
                 ))
        succ (fn [state]
               (for [dir [:up :right :down :left]
                     :let [next (walk world state dir)]
                     :when next]
                 (do
                   ;;(println "next" next "dir" dir)
                   next)))]
    (bfs start succ stop)))


(comment
  (parse example-in)
  (do (println "-----------------------------------")
      (day24 example-in))
  
  (do (println "-----------------------------------")
      (day24 (slurp (io/resource "day24.txt"))))
  ;;{:steps 460, :seen #{0 7 1 4 6 3 2 5}, :at [1 7]}
  ;; to return:
  (do (println "-----------------------------------")
      (day24-2 (slurp (io/resource "day24.txt"))))
  ;; {:steps 668, :seen #{0 7 1 4 6 3 2 5}, :at [141 23], :stage-two true, :best-stage-one {:steps 640, :seen #{0 7 1 4 6 3 2 5}, :at [133 35]}}
  ;; best2 {:steps 900, :seen #{0 7 1 4 6 3 2 5}, :at [23 33], :stage-two true, :best-stage-one {:steps 640, :seen #{0 7 1 4 6 3 2 5}, :at [133 35]}} ;; too low
  ;;{:steps 680, :seen #{0}, :at [141 23]} ;; too high
  
  (def x @ats)
  (showx (slurp (io/resource "day24.txt")) x)
  )
