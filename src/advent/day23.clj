(ns advent.day23
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
                             (s/conformer #(conj {} (:reg-or-n %))))))

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
             (println "TGL!" n reg)
             (recur (inc p)
                    registers
                    (invert ins (+ p n))))))
       ;; p outside of ins, done and return result
       registers))))

(def example-in
  "cpy 2 a
tgl a
tgl a
tgl a
cpy 1 a
dec a
dec a
")

(comment
  (s/conform ::ins example-in)
  (s/explain ::ins example-in)
  ;; => [[:cpy {:to-reg \a, :n 41}] [:inc \a] [:inc \a] [:dec \a] [:jnz {:n 2, :register \a}] [:dec \a]]


  (execute (s/conform ::ins example-in))
  (s/conform ::ins example-in)
  [[:cpy {:to-reg \a, :n 2}]
   [:tgl {:reg \a}]
   [:tgl {:reg \a}]
   [:tgl {:reg \a}]
   [:cpy {:to-reg \a, :n 1}]
   [:dec \a]
   [:dec \a]]
  
  (def d23 (s/conform ::ins (slurp (io/resource "day23.txt"))))
  
  (execute d23 {\a 7}) ;;=> {\a 10584, \b 1, \d 0, \c 0} ;;=> {\a 2, \b 5, \d 3, \c 73}

  (execute d23 {\a 12}) ;;=> {\a 479007144, \b 1, \d 0, \c 0}
  )
