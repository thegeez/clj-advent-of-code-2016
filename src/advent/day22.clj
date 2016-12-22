(ns advent.day22
  (:require [clojure.spec :as s]
            [clojure.java.io :as io]
            [clojure.string :as str]))

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

(s/def ::node (s/& (s/cat :pre (match-seq "/dev/grid/node-x")
                          :x ::number
                          :dashy (match-seq "-y")
                          :y ::number
                          :spaces (s/+ #{\space})
                          :size ::number
                          :T #{\T}
                          :spaces (s/+ #{\space})
                          :used ::number
                          :T #{\T}
                          :spaces (s/+ #{\space})
                          :avail ::number
                          :T #{\T}
                          :spaces (s/+ #{\space})
                          :use ::number
                          :percent #{\%})
                   (s/conformer #(select-keys % [:x :y :size :used :avail :use]))))

(s/def ::disk (s/and string?
                     (s/conformer seq)
                     (s/cat :pre (s/? (match-seq "root@ebhq-gridcenter# df -h\n"))
                            :pre (match-seq "Filesystem")
                            :spaces (s/+ #{\space})
                            :pre (match-seq "Size  Used  Avail  Use%")
                            :newline #{\newline}
                            :nodes (s/* (s/& (s/cat :node ::node
                                                    :newline #{\newline})
                                             (s/conformer #(:node %)))))
                     (s/conformer #(:nodes %))))

(defn day22 []
  (let [nodes (s/conform ::disk (slurp (io/resource "day22.txt")))
        pairs (for [n nodes
                    m nodes]
                [n m])]
    (count (filter
            (fn [[n m]]
              (and (not= (:used n) 0)
                   (not= n m)
                   (<= (:used n) (:avail m))))
            pairs))
    #_[(count pairs) (count nodes) (/ (* (count nodes) (dec (count nodes))) 2)]))

(comment
  (day22) ;; => 960
  (let [s (sorted-set-by (fn [l r] (< (:a l) (:a r))) {:a 2} {:a 1} {:a 4})]
    (-> s
        (into (map (fn [i] {:a i}) (reverse (range 100))))
        (conj {:a 3})))
  (s/conform ::disk (slurp (io/resource "day22.txt")))
  (s/explain ::disk (slurp (io/resource "day22.txt")))
  (subs (slurp (io/resource "day22.txt")) 75 100)
  )

(def example-in
  "Filesystem            Size  Used  Avail  Use%
/dev/grid/node-x0-y0   10T    8T     2T   80%
/dev/grid/node-x0-y1   11T    6T     5T   54%
/dev/grid/node-x0-y2   32T   28T     4T   87%
/dev/grid/node-x1-y0    9T    7T     2T   77%
/dev/grid/node-x1-y1    8T    0T     8T    0%
/dev/grid/node-x1-y2   11T    7T     4T   63%
/dev/grid/node-x2-y0   10T    6T     4T   60%
/dev/grid/node-x2-y1    9T    8T     1T   88%
/dev/grid/node-x2-y2    9T    6T     3T   66%
")

(defn bfs [start succ stop]
  (if (stop start)
    [0 start]
    (loop [doing {(:empty start) start}
           visited #{(:empty start)}
           steps 1]
      (println "steps" steps " visited " (count visited) " doing " (count doing))
      #_(println "visited" visited)
      (if (empty? doing)
        :undoable
        (let [next (transduce
                    (comp
                     (mapcat (comp succ val))
                     (keep (fn [state]
                             (cond
                               (contains? visited (:empty state))
                               nil
                               (stop state)
                               {:done state}
                               :else state)))
                     (halt-when :done)
                     (map (juxt :empty identity)))
                    conj
                    {}
                    doing)]
          (if-let [match (:done next)]
            [steps match]
            (recur next
                   (into visited (map :empty (vals next)))
                   (inc steps))))))))


;; move empty square next on path to goal
;; switch goal and empty

(defn day22-2 [nodes]
  (let [coor-nodes (zipmap
                    (map (juxt :x :y) nodes)
                    nodes)
        ;; get this data
        goal-coor (apply max-key first (filter (comp #{0} second) (keys coor-nodes)))
        ;; target-node
        target-coor [0 0]
        empty-coor (some (fn [[[x y] node]]
                           (when (= (:used node) 0)
                             [x y])) coor-nodes)
        #_ (println [goal-coor target-coor empty-coor])
        ;; state {[x y] {..node..}, :empty [x y], :goal [x y], :path [[x y] ...]}
        max-x (apply max (map first (keys coor-nodes)))
        max-y (apply max (map second (keys coor-nodes)))
        ;;_ (println "max-x" max-x)
        ;;_ (println "max-y" max-y)
        neighbours (fn [{:keys [x y]}]
                     (for [[dx dy] [[0 -1] [0 1] [-1 0] [1 0]]
                           :let [nx (+ x dx)
                                 ny (+ y dy)]
                           :when (and (<= 0 nx max-x)
                                      (<= 0 ny max-y))]
                       [nx ny]))
        succ (fn [state]
               ;;(println "succ" (:empty state))
               (let [at (get state (:empty state))]
                 ;;(println "at" at)
                 (for [[nx ny] (neighbours at)
                       :let [ ;;_ (println "nx ny" [nx ny])
                             n (get state [nx ny])
                             ;;_ (println "n" n)
                             ]
                       :when (and (<= (:used n) (:size at))
                                  (not= (:goal state) [nx ny]))]
                   (-> state
                       (assoc :empty [nx ny])
                       (update :path conj [nx ny])
                       (assoc-in [(:empty state) :used] (:used n))
                       (assoc-in [[nx ny] :used] 0)))))
        start (assoc coor-nodes
                     :empty empty-coor
                     :goal goal-coor)

        next-to-goal-coor (let [[x y] goal-coor]
                            [(dec x) y])
        next-to-goal-stop (fn [state]
                            (= (:empty state) next-to-goal-coor))
        swap-empty-goal (fn [state]
                          ;;(println "swap state" state)
                          (-> state
                              (assoc :empty (:goal state))
                              (assoc :goal (:empty state))
                              (assoc-in [(:empty state) :used] (get-in state [(:goal state) :used]))
                              (assoc-in [(:goal state) :used] 0)))
        [steps state] (bfs start succ next-to-goal-stop)
      ]
    (loop [state state
           total steps]
      (let [state (swap-empty-goal state)
            total (inc total)]
        (if (= (:goal state) [0 0])
          [total state]
          (let [goal-fn (let [[x y] (:goal state)]
                          (fn [state]
                            (= (:empty state) [(dec x) y])))
                [steps state] (bfs state succ goal-fn)]
            (recur state
                   (+ total steps))))))
    ))

(defn draw [nodes]
  (let [coor-nodes (zipmap
                    (map (juxt :x :y) nodes)
                    nodes)
        mx (apply max (map first (keys coor-nodes)))
        my (apply max (map second (keys coor-nodes)))
        pad-left (fn [s]
                   (let [s (str s)]
                     (apply str (concat (repeat (- 2 (count s)) " ")
                                        s))))
        pad-right (fn [s]
                    (let [s (str s)]
                      (apply str (concat s
                                         (repeat (- 2 (count s)) " ")))))]
    (println "----------------------------")
    (println (:goal nodes))
    (doseq [j (range (inc my))]
      (println (str/join " "
                         (map
                          (fn [i]
                            (let [n (get coor-nodes [i j])]
                              #_(str (pad-left (:used n)) "/" (pad-right (:size n)))
                              (cond
                                (= [0 0] [i j])
                                "T"
                                (:goal n)
                                "G"
                                (= (:used n) 0)
                                "_"
                                (< 110 (:size n))
                                "#"
                                :else
                                #_(:used n)
                                ".")))
                          (range (inc mx))))))))

(defn find-draw [nodes]
  (let [[steps res] (let [res (day22-2 nodes)]
                      (println "BAL" res)
                      res)
        path (set (:path res))
        mx (apply max (map :x nodes))
        my (apply max (map :y nodes))
        with-path (for [y (range (inc my))
                        x (range (inc mx))]
                    (let [n (get res [x y])]
                      (cond
                        (= (:goal res) [x y])
                        (assoc n :goal true)
                        (contains? path [x y])
                        (assoc n :used 0)
                        :else
                        n)))]
    (draw with-path)
    [steps res]))

(comment
  (draw (s/conform ::disk example-in))
  (draw (s/conform ::disk (slurp (io/resource "day22.txt"))))
  (day22-2 (s/conform ::disk example-in))
  (def nodes (s/conform ::disk (slurp (io/resource "day22.txt"))))
  (day22-2 nodes)
  (first (find-draw nodes));; => 225 
  
  (let [normal {:size 10 :used 4 :avail 6}
        large {:size 200 :used 190 :avail 10}
        empty {:size 10 :used 0 :avail 10}
        demo-map (for [x (range 8)
                       y (range 8)]
                   (merge {:x x :y y}
                          (cond
                            (and (< 0 x)
                                 (= y 3))
                            large
                            (= [x y] [3 7])
                            empty
                            :else
                            normal)))]
    (draw demo-map)
    ;;(day22-2 demo-map)
    (find-draw demo-map))
  
  (find-draw (s/conform ::disk example-in))
  (s/conform ::disk example-in)
  (s/explain ::disk example-in)
  )

