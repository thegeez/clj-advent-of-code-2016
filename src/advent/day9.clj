(ns advent.day9
  (:require [clojure.string :as str]
            [clojure.spec :as s]
            [clojure.spec.gen :as gen]
            [clojure.java.io :as io]))

(def numbers "0123456789")
(def number (set numbers))
(def alphabet "ABCDEFGHIJKLMNOPQRSTUVWXYZ")
(def letter (set alphabet))

(s/def ::number (s/& (s/+ number)
                     (s/conformer
                      (fn [p]
                        (Long/parseLong (apply str p))))))

(s/def ::ins (s/& (s/cat :bracket-open #{\(}
                         :take-n ::number
                         :x #{\x}
                         :repeat-n ::number
                         :close-bracket #{\)})
                  (s/conformer
                   #(select-keys % [:take-n :repeat-n]))))

(s/def ::ins+rest (s/cat :ins ::ins
                         :rest (s/+ char?)))
(s/conform ::number (seq "12"))
(s/conform ::ins (seq "(12x3)"))
(s/conform ::ins+rest (seq "(12x3)(1x4)ABC"))
(s/explain ::ins+rest (seq "(12x3)(1x4)ABC"))

(defn day9 [str-in]
  (let [;; remove whitespace
        in (first (str/split-lines str-in))
        _ (println "in" in)
        decoded
        (loop [in (seq in)
               out []]
          (if-not (seq in)
            out
            (let [p (s/conform ::ins+rest in)]
              #_(println "p" p)
              (if (= p ::s/invalid)
                (recur (next in) (conj out (first in)))
                (let [{{:keys [take-n repeat-n]} :ins rest :rest} p]
                  (recur (drop take-n rest)
                         (into out (mapcat identity (repeat repeat-n (take take-n rest))))))))))]
    (count decoded)))

(defn decode-count [in]
  (loop [in (seq in)
         out 0]
    (if-not (seq in)
      out
      (let [p (s/conform ::ins+rest in)]
        (if (= p ::s/invalid)
          (recur (next in) (inc out))
          (let [{{:keys [take-n repeat-n]} :ins rest :rest} p]
            (recur (drop take-n rest)
                   (+ out (* repeat-n (decode-count (take take-n rest)))))))))))

(defn day9-2 [str-in]
  (let [in (first (str/split-lines str-in))]
    (decode-count in)))

(comment
  (day9 (slurp (io/resource "day9.txt"))) ;;=> 74532
  (day9 "ADVENT")
  (day9 "A(1x5)BC")
  (day9 "(3x3)XYZ")
  (day9 "A(2x2)BCD(2x2)EFG")
  (day9 "(6x1)(1x3)A")
  (day9 "X(8x2)(3x3)ABCY")

  (day9-2 "ADVENT")
  (=  (day9-2 "X(8x2)(3x3)ABCY")
      (count "XABCABCABCABCABCABCY"))
  (= (day9-2 "(27x12)(20x12)(13x14)(7x10)(1x12)A")
     241920)
  (= (day9-2 "(25x3)(3x3)ABC(2x3)XY(5x2)PQRSTX(18x9)(3x2)TWO(5x7)SEVEN")
     445)
  (day9-2 (slurp (io/resource "day9.txt"))) ;;=> 11558231665
  )


