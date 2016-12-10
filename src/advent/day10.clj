(ns advent.day10
  (:require [clojure.string :as str]
            [clojure.spec :as s]
            [clojure.spec.gen :as gen]
            [clojure.java.io :as io]))

(def numbers "0123456789")
(def number (set numbers))
(def alphabet "abcdefghijklmnopqrstuvwxyz")
(def letter (set alphabet))

(s/def ::number (s/& (s/+ number)
                     (s/conformer
                      (fn [parsed]
                        (Long/parseLong (apply str parsed)))
                      (fn [out]
                        (seq (str out))))))

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

(s/def ::output (s/& (s/cat :output-str (match-seq "output ")
                            :number ::number)
                     (s/conformer #(:number %))))
(s/def ::bot-out (s/& (s/cat :bot-str (match-seq "bot ")
                             :number ::number)
                      (s/conformer #(:number %))))

(s/def ::in (s/alt :get-value (s/& (s/cat :value-str (match-seq "value ")
                                          :value ::number
                                          :goes-to-str (match-seq " goes to bot ")
                                          :bot ::number)
                                   (s/conformer
                                    #(select-keys % [:value :bot])))
                   :action (s/& (s/cat :bot-str (match-seq "bot ")
                                       :from-bot ::number
                                       :gives-str (match-seq " gives low to ")
                                       :low-out (s/alt :output ::output
                                                       :bot-out ::bot-out)
                                       :and-high-str (match-seq " and high to ")
                                       :high-out (s/alt :output ::output
                                                        :bot-out ::bot-out))
                                (s/conformer
                                 #(select-keys % [:from-bot :low-out :high-out])))
                   ))

(s/def ::ins (s/and string?
                    (s/conformer seq)
                    (s/+ (s/& (s/cat
                               :in ::in
                               :newline #{\newline})
                              (s/conformer #(:in %))))))


(defn day10 []
  (let [in (slurp (io/resource "day10.txt"))
        ins (s/conform ::ins in)
        res (loop [[instruction & ins] ins
                   state {}]
              (println "instr: " instruction)
              (let [[type] instruction]
                (condp = type
                  :get-value
                  (let [{:keys [value bot]} (second instruction)
                        bot-val (get-in state [bot :value])]
                    (cond
                      (= #{61 17} #{bot-val value})
                      (do
                        ;; done!
                        bot)
                      (not bot-val) ;; first of two values to bot
                      (recur ins
                             (assoc-in state [bot :value] value))
                      bot-val ;; two values in bot, execute action
                      (if-let [action (get-in state [bot :action])]
                        ;; bot knows what to do with two values!
                        (let [extra-ins
                              (let [{[low-out-type low-out-num low-out] :low-out
                                     [high-out-type high-out-num] :high-out} action
                                    [low high] (if (< value bot-val)
                                                 [value bot-val]
                                                 [bot-val value])]
                                (cond-> []
                                  (= low-out-type :bot-out)
                                  (conj [:get-value {:value low :bot low-out-num}])
                                  (= high-out-type :bot-out)
                                  (conj [:get-value {:value high :bot high-out-num}])))]
                          (recur (into extra-ins ins)
                                 (update state bot dissoc :value)))
                        ;; bot has to values but no action yet
                        (recur
                         ins
                         (assoc-in state [bot :todo] value))
                        )
                      ))

                  :action
                  (let [{:keys [from-bot low-out high-out]} (second instruction)]
                    (if-let [todo (get-in state [from-bot :todo])]
                      ;; bot has two values before its instruction
                      (recur
                       (into [[:get-value {:value todo :bot from-bot}]] ins)
                       (-> state
                           (update from-bot dissoc :todo)
                           (assoc-in [from-bot :action] {:low-out low-out
                                                         :high-out high-out})))
                      ;; bot does not have to values yet
                      (recur ins
                             (assoc-in state [from-bot :action] {:low-out low-out
                                                                 :high-out high-out})))))))]
    res))

(defn day10-2 []
  (let [in (slurp (io/resource "day10.txt"))
        ins (s/conform ::ins in)
        res (loop [[instruction & ins] ins
                   state {}]
              (let [outputs (map #(get-in state [:outputs %]) [0 1 2])]
                (if (every? identity outputs)
                  (do
                    (println "outputs: " outputs)
                    (apply * outputs))
                  (let [[type] instruction]
                    (condp = type
                      ;; [:to-output {:value low :output low-out-num}]
                      :to-output
                      (let [{:keys [value output]} (second instruction)]
                        (recur ins
                               (assoc-in state [:outputs output] value)))
                      :get-value
                      (let [{:keys [value bot]} (second instruction)
                            bot-val (get-in state [bot :value])]
                        (cond
                          (not bot-val) ;; first of two values to bot
                          (recur ins
                                 (assoc-in state [bot :value] value))
                          bot-val ;; two values in bot, execute action
                          (if-let [action (get-in state [bot :action])]
                            ;; bot knows what to do with two values!
                            (let [extra-ins
                                  (let [{[low-out-type low-out-num] :low-out
                                         [high-out-type high-out-num] :high-out} action
                                        [low high] (if (< value bot-val)
                                                     [value bot-val]
                                                     [bot-val value])]
                                    (cond-> []
                                      (= low-out-type :bot-out)
                                      (conj [:get-value {:value low :bot low-out-num}])
                                      (= high-out-type :bot-out)
                                      (conj [:get-value {:value high :bot high-out-num}])

                                      (= low-out-type :output)
                                      (conj [:to-output {:value low :output low-out-num}])
                                      (= high-out-type :output)
                                      (conj [:to-output {:value high :output high-out-num}])))]
                              (recur (into extra-ins ins)
                                     (update state bot dissoc :value)))
                            ;; bot has to values but no action yet
                            (recur
                             ins
                             (assoc-in state [bot :todo] value))
                            )
                          ))

                      :action
                      (let [{:keys [from-bot low-out high-out]} (second instruction)]
                        (if-let [todo (get-in state [from-bot :todo])]
                          ;; bot has two values before its instruction
                          (recur
                           (into [[:get-value {:value todo :bot from-bot}]] ins)
                           (-> state
                               (update from-bot dissoc :todo)
                               (assoc-in [from-bot :action] {:low-out low-out
                                                             :high-out high-out})))
                          ;; bot does not have to values yet
                          (recur ins
                                 (assoc-in state [from-bot :action] {:low-out low-out
                                                                     :high-out high-out})))))))))]
    res))

(comment
  
  (day10) ;=> 47
  (let [in (slurp (io/resource "day10.txt"))
        ins (s/conform ::ins in)]
    (take 10 ins)
    )
  ;; ([:action {:from-bot 59, :low-out [:bot-out 176], :high-out [:bot-out 120]}]
  ;;  [:action {:from-bot 92, :low-out [:bot-out 42], :high-out [:bot-out 187]}]
  ;;  [:get-value {:value 31, :bot 114}]
  ;;  [:action {:from-bot 182, :low-out [:bot-out 49], :high-out [:bot-out 176]}]
  ;;  [:action {:from-bot 17, :low-out [:bot-out 181], :high-out [:bot-out 162]}]
  ;;  [:action {:from-bot 36, :low-out [:bot-out 118], :high-out [:bot-out 121]}]
  ;;  [:action {:from-bot 118, :low-out [:bot-out 164], :high-out [:bot-out 55]}]
  ;;  [:action {:from-bot 172, :low-out [:bot-out 79], :high-out [:bot-out 123]}]
  ;;  [:action {:from-bot 51, :low-out [:bot-out 60], :high-out [:bot-out 31]}]
  ;;  [:action {:from-bot 48, :low-out [:bot-out 107], :high-out [:bot-out 58]}])

  (day10-2) ;=> 2666
  )
