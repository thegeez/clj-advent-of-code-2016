(ns advent.day12
  (:require [clojure.string :as str]
            [clojure.spec :as s]
            [clojure.java.io :as io]))

(def example-in
  "cpy 41 a
inc a
inc a
dec a
jnz a 2
dec a
")

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

(def numbers "0123456789")
(s/def ::number (s/& (s/cat :minus (s/? #{\-})
                            :ns (s/+ (set numbers)))
                     (s/conformer
                      #(* (if (:minus %) -1 1)
                          (Long/parseLong (apply str (:ns %)))))))

(def alphabet "abcdefghijklmnopqrstuvwxyz")
(def letter (set alphabet))

(s/def ::in (s/alt :cpy (s/& (s/cat :cpy-str (match-seq "cpy ")
                                    :reg-or-n
                                    (s/alt :from-reg letter
                                           :n ::number)
                                    :space #{\space}
                                    :to-reg letter)
                             (s/conformer #(conj {:to-reg (:to-reg %)}
                                                 (:reg-or-n %))))
                   :inc (s/& (s/cat :inc-str (match-seq "inc ")
                                    :register letter)
                             (s/conformer #(:register %)))
                   :dec (s/& (s/cat :inc-str (match-seq "dec ")
                                    :register letter)
                             (s/conformer #(:register %)))
                   :jnz (s/& (s/cat :cpy-str (match-seq "jnz ")
                                    :reg-or-n
                                    (s/alt :from-reg letter
                                           :n ::number)
                                    :space #{\space}
                                    :skip-n ::number)
                             (s/conformer #(conj {:skip-n (:skip-n %)}
                                                 (:reg-or-n %))))))

(s/def ::ins (s/and string?
                    (s/conformer seq)
                    (s/* (s/& (s/cat :in ::in
                                     :newline #{\newline})
                              (s/conformer #(:in %))))))

(defn execute
  ([ins] (execute ins {}))
  ([ins init-regs]
   (loop [p 0
          registers init-regs]
     (if-let [in (get ins p)]
       (let [[type args] in]
         (condp = type
           :inc
           (let [reg args]
             (recur (inc p)
                    (update registers reg (fnil inc 0))))
           :dec
           (let [reg args]
             (recur (inc p)
                    (update registers reg (fnil dec 0))))
           :cpy
           (let [{:keys [n from-reg to-reg]} args
                 n (or n
                       (get registers from-reg 0))]
             (recur (inc p)
                    (assoc registers to-reg n)))
           :jnz
           (let [{:keys [skip-n n from-reg]} args
                 n (or n
                       (get registers from-reg 0))]
             (recur (if (zero? n)
                      (inc p)
                      (+ p skip-n))
                    registers))))
       ;; p outside of ins, done and return result
       registers))))

(comment
  (s/conform ::ins example-in)
  ;; => [[:cpy {:to-reg \a, :n 41}] [:inc \a] [:inc \a] [:dec \a] [:jnz {:n 2, :register \a}] [:dec \a]]


  (execute (s/conform ::ins example-in))
  ;; => {\a 42}
  
  (->> (slurp (io/resource "day12.txt"))
       (s/conform ::ins)
       execute)
  ;;=> {\a 318083, \b 196418, \d 0, \c 0}

  ;; two
  (execute (->> (slurp (io/resource "day12.txt"))
                (s/conform ::ins)
                )
           {\c 1})
  ;; => {\c 0, \a 9227737, \b 5702887, \d 0}
  
  )
