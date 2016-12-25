(ns feldoh.advent-2016.day8
  (:use clojure.test
        feldoh.advent-2016.core)
  (:require [clojure.spec :as s]
            [clojure.spec.gen :as gen]
            [clojure.spec.test :as stest]
            [clojure.test.check.generators :as cgen]))

(def input (get-input-lines "day8.input"))

(with-test
  (defn rect
    ([width height] (fn [screen] (loop [remaining-height height
                                        [next-line & rest-screen] screen
                                        new-screen []]
                                   (let [replacement-line (flatten (vector (repeat width :on)
                                                                           (drop width next-line)))
                                         new-screen-with-new-line (conj new-screen replacement-line)]
                                     (if (= remaining-height 1)
                                       (reduce conj new-screen-with-new-line rest-screen)
                                       (recur (- remaining-height 1) rest-screen new-screen-with-new-line))))))
    ([instruction] (let [[width height] (clojure.string/split instruction #"x")]
                     (rect (parseInt width) (parseInt height)))))
  (is (= [[:on :on]
          [:off :on]] ((rect "2x1") [[:off :off]
                                     [:off :on]])))
  (is (= [[:on]] ((rect 1 1) [[:off]])))
  (is (= [[:on :off]] ((rect 1 1) [[:off :off]])))
  (is (= [[:on :off]
          [:off :on]] ((rect 1 1) [[:off :off]
                                   [:off :on]])))
  (is (= [[:on :on]
          [:off :on]] ((rect 2 1) [[:off :off]
                                   [:off :on]])))
  (is (= [[:on :on]
          [:on :on]] ((rect 2 2) [[:off :off]
                                  [:off :off]])))
  (is (= [[:on :on :on :on :on :on :on :on :on :on :on :on :off :on]
          [:on :on :on :on :on :on :on :on :on :on :on :on :off :on]
          [:on :off :off :off :off :off :off :off :off :off :off :off :off :on]]
         ((rect "12x2") [[:off :off :off :off :off :off :off :off :off :off :off :off :off :on]
                         [:off :off :off :off :off :off :off :off :off :off :off :off :off :on]
                         [:on :off :off :off :off :off :off :off :off :off :off :off :off :on]]))))


(s/def ::screen (s/with-gen
                  (s/and (s/coll-of (s/coll-of
                                     ;;#{:on :off}
                                     keyword?
                                     ))
                         (fn [p]
                           (apply = (map count p))))
                  (fn []
                    (gen/bind (gen/tuple cgen/s-pos-int
                                         cgen/s-pos-int)
                              (fn [[w h]]
                                (gen/return (->> (map #(keyword (str "k" %)) (range))
                                                 (take (* w h))
                                                 (partition w)))))
                    #_(gen/bind (gen/tuple cgen/s-pos-int
                                           cgen/s-pos-int)
                                (fn [[w h]]
                                  (gen/vector
                                   (gen/vector
                                    (gen/elements [:on :off])
                                    w)
                                   h))))))

(s/def ::zero-or-pos (s/with-gen
                       (fn [i]
                         (<= 0 i))
                       (fn []
                         cgen/pos-int)))

(s/fdef ::my-rot-row
        :args (s/and (s/cat :screen ::screen
                            :row-num ::zero-or-pos
                            :amount ::zero-or-pos)
                     (fn [{:keys [screen row-num h]}]
                       (< row-num (count screen))))
        :ret ::screen
        :fn (fn [{:keys [args ret]}]
              (let [row-num (:row-num args)
                    amount (:amount args)]
                (and (= (count (first (:screen args))) (count (first ret)))
                     (= (count (:screen args)) (count ret))

                     ;; not rotated row stay the same
                     (apply = (map
                               (fn [idx row-arg row-ret]
                                 (if (= idx row-num)
                                   true ;; skip for compare
                                   (= row-arg row-ret)))
                               (range)
                               (:screen args)
                               ret))
                     ;; row is rotated
                     (let [row-arg (nth (:screen args) row-num)
                           row-ret (nth ret row-num)]
                       (= (let [[pre post] (split-at (- (count row-arg)
                                                        (mod (:amount args) (count (first (:screen args))))) row-arg)]
                            (-> []
                                (into post)
                                (into pre)))
                          row-ret)))))
        )

(defn my-rot-row [screen row-num amount]
  (let [[pre-rows [row & post-rows]] (split-at row-num screen)
        row-len (count row)
        cycled-row (reverse (take row-len (drop amount (cycle (reverse row)))))]
    (reduce conj post-rows (conj pre-rows cycled-row)))
  ;; good
  #_(let [[pre-rows [row & post-rows]] (split-at row-num screen)
          row-len (count row)
          cycled-row (reverse (take row-len (drop amount (cycle (reverse row)))))]
      (-> (vec pre-rows)
          (conj cycled-row)
          (into post-rows))))

(comment
  (stest/check-fn my-rot-row ::my-rot-row)

  (gen/generate (s/gen ::screen))
  )

(with-test
  (defn rot-row [row-num amount]
    (fn [screen] (let [[pre-rows [row & post-rows]] (split-at row-num screen)
                       row-len (count row)
                       cycled-row (reverse (take row-len (drop amount (cycle (reverse row)))))]
                   (reduce conj post-rows (conj pre-rows cycled-row)))))
  (is (= [[:1 :2 :3]] ((rot-row 0 3) [[:1 :2 :3]])))
  (is (= [[:3 :1 :2]] ((rot-row 0 1) [[:1 :2 :3]])))
  (is (= [[:1 :2 :3]
          [:5 :6 :4]] ((rot-row 1 2) [[:1 :2 :3]
                                      [:4 :5 :6]])))
  (is (= [[:1 :2 :3 :4]
          [:7 :8 :5 :6]
          [:9 :a :b :c]] ((rot-row 1 2) [[:1 :2 :3 :4]
                                         [:5 :6 :7 :8]
                                         [:9 :a :b :c]])))

  (is (= [[:1 :2]
          [:3 :4]
          [:6 :5]] ((rot-row 2 2) [[:1 :2]
                                   [:3 :4]
                                   [:5 :6]]))))

(with-test
  (defn rot-column [col-num amount]
    (comp transpose (rot-row col-num amount) transpose))
  (is (= [[:1] [:2] [:3]] ((rot-column 0 3) [[:1] [:2] [:3]])))
  (is (= [[:3] [:1] [:2]] ((rot-column 0 1) [[:1] [:2] [:3]])))
  (is (= [[:1 :5 :3]
          [:4 :2 :6]] ((rot-column 1 1) [[:1 :2 :3]
                                         [:4 :5 :6]])))
  (is (= [[:1 :5 :3]
          [:4 :8 :6]
          [:7 :2 :9]] ((rot-column 1 2) [[:1 :2 :3]
                                         [:4 :5 :6]
                                         [:7 :8 :9]])))

  (is (= [[:1 :8 :3 :e :i]
          [:4 :b :6 :f :j]
          [:7 :2 :9 :g :k]
          [:a :5 :c :h :l]] ((rot-column 1 2) [[:1 :2 :3 :e :i]
                                               [:4 :5 :6 :f :j]
                                               [:7 :8 :9 :g :k]
                                               [:a :b :c :h :l]]))))
(defn rotate [axis pos _ rotation]
  (let [fun-as-symbol (symbol (str "rot-" axis))
        parsed-position (second (clojure.string/split pos #"="))]
    ;;(println "rotate" fun-as-symbol parsed-position rotation)
    ((resolve fun-as-symbol) (parseInt parsed-position) (parseInt rotation))))

(with-test
  (defn parse-instruction [instruction]
    (let [[action & args] (clojure.string/split instruction #" ")]
      ;;(println "action" action)
      (apply (resolve (symbol action)) (apply vector args))))
  (is (= [[:on :on :on]] ((parse-instruction "rect 3x1") [[:off :off :off]])))
  (is (= [[:2 :3 :1]] ((parse-instruction "rotate row y=0 by 2") [[:1 :2 :3]])))
  (is (= [[:a :b :c] [:2 :3 :1]] ((parse-instruction "rotate row y=1 by 2") [[:a :b :c] [:1 :2 :3]])))
  (is (= [[:2] [:3] [:1]] ((parse-instruction "rotate column x=0 by 2") [[:1] [:2] [:3]]))))

(with-test
  (defn make-screen [width height] (repeat height (repeat width :off)))
  (is (= [[:off :off :off] [:off :off :off]] (make-screen 3 2))))

(with-test
  (defn apply-instructions [screen instructions]
    (reduce (fn [acc inst] ((parse-instruction inst) acc)) screen (take 29 instructions)))
  (is (= [[:off :on :off :off :on :off :on]
          [:on :off :on :off :off :off :off]
          [:off :on :off :off :off :off :off]] (apply-instructions
                                                 (make-screen 7 3) ["rect 3x2"
                                                                    "rotate column x=1 by 1"
                                                                    "rotate row y=0 by 4"
                                                                    "rotate column x=1 by 1"]))))

(def screen-50-by-6 (make-screen 50 6))
(def part-1-screen (apply-instructions screen-50-by-6 input))
(count (filter #(= :on %) (flatten part-1-screen)))

;;(nth input 29) ;;"rotate column x=32 by 1"
;; =>100

#_(run-tests)

(doall (map println (mapv #(mapv {:on \# :off \.} %) (apply-instructions screen-50-by-6 input))))

;; (def mine
;;   '[[# # # # . . . # # . # . . # . # # # . . # . . # . . # # . . # # # . . # . . . . # . . . # . . # # .]
;;     [. . . # . . . . # . # . . # . # . . # . # . # . . # . . # . # . . # . # . . . . # . . . # . . . # .]
;;     [. . # . . . . . # . # # # # . # . . # . # # . . . # . . . . # . . # . # . . . . . # . # . . . . # .]
;;     [. # . . . . . . # . # . . # . # # # . . # . # . . # . . . . # # # . . # . . . . . . # . . . . . # .]
;;     [# . . . . # . . # . # . . # . # . # . . # . # . . # . . # . # . . . . # . . . . . . # . . # . . # .]
;;     [# # # # . . # # . . # . . # . # . . # . # . . # . . # # . . # . . . . # # # # . . . # . . . # # . .]])


;; ;; (println (mapv #(mapv {:on \# :off \.} %) r))
;; (comment
;;   (def their
;;    '[[. . # # . . . . . # # # # # # # # . # . . . # . . . # . . # . # . . . . . . # # # # . . . . # . # .]
;;      [. . . . . . # . . # . # . # . . # . # . . . . # . # # . # # # # . . # . . . . # . . . . . . . . # .]
;;      [. . . . # # . . # . . . . # # . . . . . . . . . # # . # # . # # . # . . . . . . . # . # . # # . . .]
;;      [. # . . . . # . . . # . # . . . . . . . # # . . . . . # . # . . . . . . . . . . . . . # # . # . . .]
;;      [# . # . . . . . # # . # . # . . # # . # . # . . . . . . . . . . . . . # . # . # # . . . # . . . # #]
;;      [# . # . . # . # . . . # . # . . # # # # . . . . # # # # # . . . . . . . . . # . . . # . . # # . . .]
;;      ]
;;    ))
