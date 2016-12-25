(ns advent.day25
  (:require [clojure.string :as str]
            [clojure.spec :as s]
            [clojure.java.io :as io]))

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
                   ;; was [reg-or-n n skip-n] in day 12, now [reg-or-n reg-or-n]
                   :jnz (s/& (s/cat :cpy-str (match-seq "jnz ")
                                    :from-reg-or-n
                                    (s/alt :from-reg letter
                                           :from-n ::number)
                                    :space #{\space}
                                    :to-reg-or-n
                                    (s/alt :to-reg letter
                                           :to-n ::number))
                             (s/conformer #(merge (conj {} (:from-reg-or-n %))
                                                  (conj {} (:to-reg-or-n %)))))
                   :tgl (s/& (s/cat :toggle (match-seq "tgl ")
                                    :reg-or-n (s/alt :reg letter
                                                     :n ::number))
                             (s/conformer #(conj {} (:reg-or-n %))))
                   :out (s/& (s/cat :out (match-seq "out ")
                                    :reg letter)
                             (s/conformer #(select-keys % [:reg])))))

(s/def ::ins (s/and string?
                    (s/conformer seq)
                    (s/* (s/& (s/cat :in ::in
                                     :newline #{\newline})
                              (s/conformer #(:in %))))))

(defn invert [ins n]
  (if (not (<= 0 n (dec (count ins))))
    ;; n outside of ins count
    ins
    (update ins n
            (fn [in]
              (let [[type args] in]
                (condp = type
                  :inc ;; :register
                  [:dec args]
                  :dec ;; :register
                  [:inc args]
                  :cpy ;; :from-reg | :n, :to-reg
                  (let [{:keys [from-reg n to-reg]} args
                        ;; from-reg | from-n,  to-reg | to-n
                        first-arg (if n
                                    {:from-n n}
                                    {:from-reg from-reg})
                        second-arg {:to-reg to-reg}]
                    [:jnz (merge first-arg second-arg)])
                  :jnz ;; from-reg | from-n,  to-reg | to-n
                  (let [{:keys [from-reg from-n to-reg to-n]} args]
                    (if to-n
                      ;; can't do :cpy into an n, this is a skip:
                      [:jnz {:from-n 0 :to-n 0}]
                      (let [;; :from-reg | :n, :to-reg
                            first-arg (if from-reg
                                        {:from-reg from-reg}
                                        {:n from-n})
                            second-arg {:to-reg to-reg}]
                        [:cpy (merge first-arg second-arg)])))
                  :tgl ;; :reg | :n
                  (let [{:keys [reg n]} args]
                    (if n
                      ;; can only inc a register
                      [:jnz {:from-n 0 :to-n 0}] ;; skip
                      [:inc reg]))))))))

(defn execute
  ([ins] (execute ins {}))
  ([ins init-regs]
   (loop [p 0
          registers init-regs
          ins ins]
     #_(do (println "p" p "registers" registers)
           (println "ins")
           (doseq [in ins]
             (println "   " in)))
     (if-let [in (get ins p)]
       (let [[type args] in]
         (condp = type
           :inc
           (let [reg args]
             (recur (inc p)
                    (update registers reg (fnil inc 0))
                    ins))
           :dec
           (let [reg args]
             (recur (inc p)
                    (update registers reg (fnil dec 0))
                    ins))
           :cpy
           (let [{:keys [n from-reg to-reg]} args
                 n (or n
                       (get registers from-reg 0))]
             (recur (inc p)
                    (assoc registers to-reg n)
                    ins))
           :jnz
           (let [{:keys [from-reg from-n to-reg to-n]} args
                 n (or from-n
                       (get registers from-reg 0))
                 skip-n (or to-n
                            (get registers to-reg 0))]
             (recur (if (zero? n)
                      (inc p)
                      (+ p skip-n))
                    registers
                    ins))
           :tgl
           (let [{:keys [reg n]} args
                 n (or n
                       (get registers reg 0))]
             (recur (inc p)
                    registers
                    (invert ins (+ p n))))
           :out
           (let [{:keys [reg]} args
                 n (get registers reg 0)
                 registers (update registers :out (fnil conj []) n)
                 out (:out registers)]
             (if (= (count out) 30)
               (if (= (:out registers) (take 30 (cycle [0 1])))
                 (assoc registers :match true)
                 registers)
               (recur (inc p)
                      registers
                      ins)))))
       ;; p outside of ins, done and return result
       registers))))

(defn day25 [str-in]
  (let [ins (s/conform ::ins str-in)
        match (some
               (fn [i]
                 (println "trying " i)
                 (let [res (execute ins {\a i})]
                   (println "res" res)
                   (when (:match res)
                     res)))
               (range))]
    match))

(comment
  (s/conform ::ins (slurp (io/resource "day25.txt")))
  (s/explain ::ins (slurp (io/resource "day25.txt")))
  (execute (s/conform ::ins (slurp (io/resource "day25.txt"))))

  (day25 (slurp (io/resource "day25.txt"))){\a 42, \d 2730, \c 0, \b 1, :out [0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1], :match true}
  ;;=> {\a 42, \d 2730, \c 0, \b 1, :out [0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1], :match true}
  )
