(ns advent.day2
  (:require [clojure.spec :as s]
            [clojure.test :as test]
            [clojure.java.io :as io]))



(def t1 "ULL
RRDDD
LURDL
UUUUD
")

(s/def ::step (s/+ #{\R \L \U \D}))
(s/def ::steps (s/and string?
                      (s/conformer seq)
                      (s/+ (s/cat :step (s/+ #{\R \L \U \D})
                                  :newline #{\newline}))
                      ;; bug fix:
                      ;;http://dev.clojure.org/jira/browse/CLJ-2003
                      ;; see first comment
                      #_(s/conformer
                       (fn [[x xs :as s]]
                         (println "S" s)
                         (into [x] xs)))
                      (s/conformer
                       #(map :step %))))
(comment
  (s/conform ::steps t1)
  )

(def buttons
  {1 {\D 4 \R 2}      2 {\L 1 \D 5 \R 3}       3 {\L 2 \D 6}
   4 {\U 1 \R 5 \D 7} 5 {\L 4 \U 2 \R 6 \D 8}  6 {\L 5 \U 3 \D 9}
   7 {\U 4 \R 8}      8 {\L 7 \U 5 \R 9}       9 {\L 8 \U 6}})

(defn code [str-in]
  (let [steps (s/conform ::steps str-in)
        res (reduce
             (fn [{:keys [from code]} step]
               (let [c (reduce
                        (fn [at dir]
                          (get-in buttons [at dir] at))
                        from
                        step)]
                 {:from c
                  :code (conj code c)}))
             {:from 5
              :code []}
             steps)]
    (apply str (:code res))
    ))

(test/deftest tests
  (test/is (= (code t1) "1985")))

(defn day2 []
  (let [in (slurp (io/resource "day2.txt"))]
    (code in)))

(comment
  (tests)
  (let [in (slurp (io/resource "day2.txt"))
        ]
    (println (count (line-seq in)))
    (line-seq in)
    #_(s/conform ::steps in)
    #_(s/explain ::steps in))
  (day2) ;; => "24862"
  )
;;     1
;;   2 3 4
;; 5 6 7 8 9
;;   A B C
;;     D
(def buttons2
  {                                  1 {\D 3}
            2 {\R 3 \D 6}            3 {\L 2 \U 1 \R 4 \D 7}     4 {\L 3 \D 8}
   5 {\R 6} 6 {\L 5 \U 2 \R 7 \D \A} 7 {\L 6 \U 3 \R 8 \D \B}    8 {\L 7 \U 4 \R 9 \D \C} 9 {\L 8}
           \A {\U 6 \R \B}          \B {\L \A \U 7 \R \C \D \D}  \C {\L \B \U 8}
                                    \D {\U \B}})

(defn code2 [str-in]
  (let [steps (s/conform ::steps str-in)
        _ (println "steps" steps)
        res (reduce
             (fn [{:keys [from code]} step]
               (let [c (reduce
                        (fn [at dir]
                          (get-in buttons2 [at dir] at))
                        from
                        step)]
                 {:from c
                  :code (conj code c)}))
             {:from 5
              :code []}
             steps)]
    (apply str (:code res))
    ))

(test/deftest tests2
  (test/is (= (code2 t1) "5DB3")))

(defn day2-1 []
  (let [in (slurp (io/resource "day2.txt"))]
    (code2 in)))

(comment
  (tests2)
  (day2-1);; "46C91"
 )
