(ns advent.day17
  (:require digest
            [clojure.test :as test]))

(def cut (atom -1))

(defn bfs [start succ stop]
  (if (stop start)
    [0 start]
    (loop [doing #{start}
           steps 1]
      ;;(println "steps" steps " visited " (count visited) " doing " (count doing))
      ;;(println "visited" visited)
      (let [next (transduce
                  (comp
                   (mapcat succ)
                   (halt-when stop
                              (fn [r i]
                                {:done i})))
                  conj
                  #{}
                  doing)]
        (if (zero? (swap! cut dec))
          (do
            (println "next")
            (println next)
            next)
          (if-let [match (:done next)]
                 [steps match]
                 (recur next
                        (inc steps))))))))

(def doors {0 #{"D" "R"},  1 #{"R" "D" "L"},  2 #{"R" "D" "L"},  3 #{"D" "L"}
            4 #{"U" "R" "D"},  5 #{"U" "R" "D" "L"}, 6 #{"U" "R" "D" "L"},  7 #{"U" "D" "L"}
            8 #{"U" "R" "D"},  9 #{"U" "R" "D" "L"}, 10 #{"U" "R" "D" "L"},  11 #{"U" "D" "L"}
            12 #{"U" "R"},  13 #{"U" "R" "L"}, 14 #{"U" "R" "L"}, 15 #{"U" "R" "L"}})

(defn dist-step [from step]
  (let [possible-steps (get doors from)]
    (when (contains? possible-steps step)
      (let [ds (get {"U" -4
                     "D"  4
                     "L" -1
                     "R"  1} step)
            to (+ from ds)]
        (when (<= 0 to 15)
          to)))))

(defn day17 [passcode]
  (let [start ["" 0] ;; path + location
        succ (fn [[path loc]]
               (let [[up down left right :as h] (digest/md5 (str passcode path))]
                 (for [[dir step] [[up "U"] [down "D"] [left "L"] [right "R"]]
                       :when (contains? (set "bcdef") dir)
                       :let [next-loc (dist-step loc step)]
                       :when next-loc]
                   [(str path step) next-loc])))
        done (fn [[path loc]]
               (when (= loc 15)
                 path))
        [steps [path loc]] (let [res (bfs start succ done)]
                             (println "res" res)
                             res)]
    path))

(test/deftest check
  (test/are [in out] (= (do
                          (reset! cut (inc (count out)))
                          (day17 in)) out)
    
    "ihgpwlah" "DDRRRD"
    "kglvqrro" "DDUDRLRRUDRD"
    "ulqzkmiv" "DRURDRUDDLLDLUURRDULRLDUUDDDRR"
    ))

(comment
  (check)
  (day17 "ihgpwlah")
  (day17 "kglvqrro")
  (day17 "lpvhkcbi");; => "DUDRLRRDDR"
    )


(defn longest [passcode]
  (let [start ["" 0] ;; path + location
        succ (fn [[path loc]]
               (let [[up down left right :as h] (digest/md5 (str passcode path))]
                 (for [[dir step] [[up "U"] [down "D"] [left "L"] [right "R"]]
                       :when (contains? (set "bcdef") dir)
                       :let [next-loc (dist-step loc step)]
                       :when next-loc]
                   [(str path step) next-loc])))
        done (fn [[path loc]]
               (when (= loc 15)
                 path))]
    (loop [doing #{start}
           best nil
           steps 1]
      (let [next (into #{}
                       (mapcat succ)
                       doing)]
        (if-not (seq next)
          best
          (recur (remove done next)
                 (or (some (fn [state]
                             (when (done state)
                               steps)) next)
                     best)
                 (inc steps)))))))

(test/deftest check2
  (test/are [in out] (= (longest in) out)
    "ihgpwlah" 370
    "kglvqrro" 492
    "ulqzkmiv" 830
    ))

(comment
  (check2)
  (longest "ihgpwlah")
  (longest "lpvhkcbi");; => 788 
  )
