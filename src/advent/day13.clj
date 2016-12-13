(ns advent.day12)

;; x*x + 3*x + 2*x*y + y + y*y
;; + input
;; count bits in binary representation
(defn open-or-wall-input [input]
  (fn [[x y]]
   (let [f (+ (* x x)
              (* 3 x)
              (* 2 x y)
              y
              (* y y))
         f (+ f input)
         fs (Long/toString f 2)
         one-count (count (filter #{\1} fs))]
     (if (even? one-count)
       \. ;;open
       \# ;;wall
       ))))


(comment
  (doseq [row (range 10)]
    (println (apply str (map #((open-or-wall-input 10) [% row]) (range 10))))
    )
  )

(defn bfs [start succ stop]
  (if (stop start)
    [0 start]
    (loop [doing #{start}
           visited #{start}
           steps 1]
      (println "steps" steps " visited " (count visited) " doing " (count doing))
      #_(println "visited" visited)
      (let [next (transduce
                  (comp
                   (mapcat succ)
                   (keep (fn [state]
                           (cond
                             (stop state)
                             {:done state}
                             (contains? visited state)
                             nil
                             :else state)))
                   (halt-when :done))
                  conj
                  #{}
                  doing)]
        (if-let [match (:done next)]
          [steps match]
          (recur next
                 (into visited next)
                 (inc steps)))))))


(defn day12 []
  (let [;;input 10
        input 1358
        open-or-wall (memoize
                      (open-or-wall-input input))
        start [1 1]
        succ (fn [[x y]]
               (for [[dx dy] [[-1 0] [1 0] [0 -1] [0 1]]
                     :let [[i j :as s] (mapv + [x y] [dx dy])]
                     :when (and (not (neg? i))
                                (not (neg? j)))
                     :when (= \. (open-or-wall s))]
                 s))
        ;;stop #{[7 4]}
        stop #{[31 39]}
        ]
    (bfs start succ stop))
  )
(comment
  (day12)[96 [31 39]]
  )

(defn bfs-2 [start succ]
  (loop [doing #{start}
         visited #{start}
         steps 0]
    (if (= steps 50)
      (count visited)
      (do
        (println "steps" steps " visited " (count visited) " doing " (count doing))
        #_(println "visited" visited)
        (let [next (transduce
                    (comp
                     (mapcat succ)
                     (keep (fn [state]
                             (cond
                               (contains? visited state)
                               nil
                               :else state)))
                     (halt-when :done))
                    conj
                    #{}
                    doing)]
          (recur next
                 (into visited next)
                 (inc steps)))))))

(defn day12-2 []
  (let [ ;;input 10
        input 1358
        open-or-wall (memoize
                      (open-or-wall-input input))
        start [1 1]
        succ (fn [[x y]]
               (for [[dx dy] [[-1 0] [1 0] [0 -1] [0 1]]
                     :let [[i j :as s] (mapv + [x y] [dx dy])]
                     :when (and (not (neg? i))
                                (not (neg? j)))
                     :when (= \. (open-or-wall s))]
                 s))]
    (bfs-2 start succ)))
(comment
  (day12-2)
  )
