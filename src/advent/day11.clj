(ns advent.day11
  (:require [clojure.string :as str]
            [clojure.spec :as s]))

(def day11-in
  "The first floor contains a promethium generator and a promethium-compatible microchip.
The second floor contains a cobalt generator, a curium generator, a ruthenium generator, and a plutonium generator.
The third floor contains a cobalt-compatible microchip, a curium-compatible microchip, a ruthenium-compatible microchip, and a plutonium-compatible microchip.
The fourth floor contains nothing relevant.
")

(def example-in
  "The first floor contains a hydrogen-compatible microchip and a lithium-compatible microchip.
The second floor contains a hydrogen generator.
The third floor contains a lithium generator.
The fourth floor contains nothing relevant.
")

(def example-steps
  ["F4 .  .  .  .  .  
F3 .  .  .  LG .  
F2 .  HG .  .  .  
F1 E  .  HM .  LM "

"F4 .  .  .  .  .  
F3 .  .  .  LG .  
F2 E  HG HM .  .  
F1 .  .  .  .  LM"

   "F4 .  .  .  .  .  
F3 E  HG HM LG .  
F2 .  .  .  .  .  
F1 .  .  .  .  LM "

   "F4 .  .  .  .  .  
F3 .  HG .  LG .  
F2 E  .  HM .  .  
F1 .  .  .  .  LM "

   "F4 .  .  .  .  .  
F3 .  HG .  LG .  
F2 .  .  .  .  .  
F1 E  .  HM .  LM "

   "F4 .  .  .  .  .  
F3 .  HG .  LG .  
F2 E  .  HM .  LM 
F1 .  .  .  .  .  "

   "F4 .  .  .  .  .  
F3 E  HG HM LG LM 
F2 .  .  .  .  .  
F1 .  .  .  .  .  "

   "F4 E  .  HM .  LM 
F3 .  HG .  LG .  
F2 .  .  .  .  .  
F1 .  .  .  .  .  "

   "F4 .  .  .  .  LM 
F3 E  HG HM LG .  
F2 .  .  .  .  .  
F1 .  .  .  .  .  "

   "F4 E  HG .  LG LM 
F3 .  .  HM .  .  
F2 .  .  .  .  .  
F1 .  .  .  .  .  "

   "F4 .  HG .  LG .  
F3 E  .  HM .  LM 
F2 .  .  .  .  .  
F1 .  .  .  .  .  "

   "F4 E  HG HM LG LM 
F3 .  .  .  .  .  
F2 .  .  .  .  .  
F1 .  .  .  .  .  "])


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

(def alphabet "abcdefghijklmnopqrstuvwxyz")
(def letter (set alphabet))

(s/def ::item (s/&
               (s/cat :a (match-seq "a ")
                      :item (s/alt :generator (s/cat :name (s/+ letter)
                                                     :gen (match-seq " generator"))
                                   :chip (s/cat :name (s/+ letter)
                                                :chip (match-seq "-compatible microchip"))))
               (s/conformer (fn [p]
                              (-> p
                                  :item
                                  (update 1 (comp #(keyword (apply str %)) :name)))))))

(s/def ::items (s/&
                (s/cat :item ::item
                       :rest (s/?
                              (s/cat
                               :others (s/* (s/cat
                                             :comma (match-seq ", ")
                                             :item ::item))
                               :maybe-comma (s/? #{\,})
                               :and (match-seq " and ")
                               :last ::item)))
                (s/conformer
                 (fn [p]
                   (-> [(:item p)]
                       (into (map :item (:others (:rest p))))
                       (into (when-let [l (:last (:rest p))]
                               [l])))))))

(defmacro floor [floor]
  `(s/& (s/cat :pre (match-seq ~(str  "The " floor " floor contains "))
               :items ::items
               :period #{\.})
        (s/conformer #(:items %))))

(s/def ::lab
  (s/and string?
         (s/conformer seq)
         (s/cat :first-floor (floor "first")
                :newline #{\newline}
                :second-floor (floor "second")
                :newline #{\newline}
                :third-floor (floor "third")
                :newline #{\newline}
                :fourth-floor (s/& (match-seq "The fourth floor contains nothing relevant.")
                                   (s/conformer (constantly [])))
                :newline #{\newline}
                )
         (s/conformer
          (fn [p]
            {:e :F1 ;; elevator
             :F4 #{}
             :F3 (set (:third-floor p))
             :F2 (set (:second-floor p))
             :F1 (set (:first-floor p))}))))

(def next-floors {:F4 '(:F3)
                  :F3 '(:F4 :F2)
                  :F2 '(:F1 :F3)
                  :F1 '(:F2)})

(defn select-elevator-items [items]
  (let [items (vec items)
        nitems (count items)]
    (into (for [i items]
            [i])
          (for [i (range nitems)
                j (range (inc i) nitems)
                :let [a (nth items i)
                      b (nth items j)]
                ;; do not put chip and different generator together
                :when (or (= (second a) (second b))
                          (= (first a) (first b)))]
            [(nth items i) (nth items j)]))))

(defn no-interference? [items]
  (or (not (some (comp #{:generator} first) items))
      (every?
       (fn [[_ elem]]
         (some #{[:generator elem]} items))
       (filter (comp #{:chip} first) items))))

(defn allowed? [lab from-floor to-floor]
  (and (no-interference? (get lab from-floor))
       (no-interference? (get lab to-floor))))

(defn move [lab to-floor take-items]
  (let [e (:e lab)]
    (-> lab
        (assoc :e to-floor)
        ;; empty elevator into curr floor and take-items
        (update-in [e] (fn [fc]
                         (into #{} (remove (set take-items)) fc)))
        (update-in [to-floor] into take-items))))

(defn lab-succ [state]
  (let [e (:e state)]
    (doall
     (for [to-floor (next-floors e)
           take-items (select-elevator-items (e state))
           :let [_ (when (not (seq take-items))
                     (throw (ex-info "WTF??" {:take-items take-items
                                              :state state
                                              })))
                 state' (move state to-floor take-items)]
           :when (allowed? state' e to-floor)]
       state'))))

(defn lab-stop [lab]
  (let [{:keys [e F1 F2 F3]} lab]
    (and (= e :F4)
         (= F3 F2 F1 #{}))))

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

(comment
  (s/conform ::lab example-in)

  (bfs (s/conform ::lab day11-in)
       lab-succ
       lab-stop)
  ; => [33 {:e :F4, :F4 #{[:chip :curium] [:chip :promethium] [:chip :plutonium] [:chip :ruthenium] [:generator :promethium] [:generator :plutonium] [:generator :ruthenium] [:generator :curium] [:generator :cobalt] [:chip :cobalt]}, :F3 #{}, :F2 #{}, :F1 #{}}]

  (bfs (update-in (s/conform ::lab day11-in)
                  [:F1] into [[:generator :elerium]
                              [:chip :elerium]
                              [:generator :dilithium]
                              [:chip :dilithium]])
       lab-succ
       lab-stop) ;=> [57 {:e :F4, :F4 #{[:chip :curium] [:chip :promethium] [:chip :elerium] [:chip :plutonium] [:generator :dilithium] [:chip :dilithium] [:chip :ruthenium] [:generator :promethium] [:generator :elerium] [:generator :plutonium] [:generator :ruthenium] [:generator :curium] [:generator :cobalt] [:chip :cobalt]}, :F3 #{}, :F2 #{}, :F1 #{}}]
  
  (bfs (s/conform ::lab example-in)
       lab-succ
       lab-stop)
  ;; => [11 {:e [:F4 [[:chip :hydrogen] [:chip :lithium]]], :F4 [[:generator :lithium] [:generator :hydrogen]], :F3 [], :F2 [], :F1 []}]
  (def e (s/conform ::lab example-in))
  (def s (lab-succ e))
  (transduce
   (comp
    (mapcat (fn [i]
              (println i)
              ((juxt inc dec) i)))
    (map (fn [i]
           (println "map" i)
           (if (= i 3)
             {:done :haha}
             i))
         )
    (keep (fn [i]
            (if (= i 1)
              1
              nil)))
    (halt-when :done))
   conj
   []
   [1 2 3 4])


  (select-elevator-items [[:chip :a] [:gen :a]])
  )
