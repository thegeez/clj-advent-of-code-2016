(ns advent.day21
  (:require [clojure.spec :as s]
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

(def number (set "0123456789"))
(s/def ::number (s/& (s/+ number)
                     (s/conformer
                      #(Long/parseLong (apply str %)))))

(def alphabet "abcdefghijklmnopqrstuvwxyz")
(s/def ::letter (set alphabet))

(s/def ::in (s/alt
             ;; swap position X with position Y
             :swap-pos (s/& (s/cat :pre (match-seq "swap position ")
                                   :from ::number
                                   :mid (match-seq " with position ")
                                   :to ::number)
                            (s/conformer #(select-keys % [:from :to])))
             ;; swap letter X with letter Y
             :swap-letter (s/& (s/cat :pre (match-seq "swap letter ")
                                      :from ::letter
                                      :mid (match-seq " with letter ")
                                      :to ::letter)
                               (s/conformer #(select-keys % [:from :to])))
             ;;rotate left/right X steps
             :rotate-dir (s/& (s/cat :pre (match-seq "rotate ")
                                     :dir (s/& (s/alt :left (match-seq "left")
                                                      :right (match-seq "right"))
                                               (s/conformer #(first %)))
                                     :space #{\space}
                                     :steps ::number
                                     :end (match-seq " step")
                                     :s (s/? #{\s}))
                              (s/conformer #(select-keys % [:dir :steps])))
             ;;rotate based on position of letter X
             :rotate-pos (s/& (s/cat :pre (match-seq "rotate based on position of letter ")
                                     :letter ::letter)
                              (s/conformer #(select-keys % [:letter])))
             ;;reverse positions X through Y
             :reverse (s/& (s/cat :pre (match-seq "reverse positions ")
                                  :from ::number
                                  :mid (match-seq " through ")
                                  :to ::number)
                           (s/conformer #(select-keys % [:from :to])))
             ;; move position X to position Y
             :move (s/& (s/cat :pre (match-seq "move position ")
                               :from ::number
                               :mid (match-seq " to position ")
                               :to ::number)
                        (s/conformer #(select-keys % [:from :to])))
             ))

(s/def ::ins (s/and string?
                    (s/conformer seq)
                    (s/* (s/& (s/cat :in ::in
                                     :newline #{\newline})
                              (s/conformer #(:in %))))))


(comment
  (s/conform ::ins (slurp (io/resource "day21.txt")))
  (s/explain ::ins (slurp (io/resource "day21.txt")))
  (subs (slurp (io/resource "day21.txt")) 1450)

  (s/explain
   #_s/conform (s/& (s/cat :pre (match-seq "rotate ")
                                     :dir (s/& (s/alt :left (match-seq "left")
                                                      :right (match-seq "right"))
                                               (s/conformer #(first %)))
                                     :space #{\space}
                                     :steps ::number
                                     :end (match-seq " steps"))
                  (s/conformer #(select-keys % [:dir :steps])))
   (seq "rotate right 4 steps"))

  )

(def example-in
  "swap position 4 with position 0
swap letter d with letter b
reverse positions 0 through 4
rotate left 1 step
move position 1 to position 4
move position 3 to position 0
rotate based on position of letter b
rotate based on position of letter d
")

(comment
  (s/conform ::ins example-in)
  (s/explain ::ins example-in)
  )

(defmulti instruction (fn [s [type args]]
                        type))

(defmethod instruction
  :swap-pos
  [s [_ {:keys [from to]}]]
  (assoc s
         to (get s from)
         from (get s to)))

(defmethod instruction
  :swap-letter
  [s [_ {:keys [from to]}]]
  (into []
        (map (fn [c]
               (cond
                 (= from c)
                 to
                 (= to c)
                 from
                 :else
                 c)))
        s))

(defmethod instruction
  :rotate-dir
  [s [_ {:keys [dir steps]}]]
  (let [idx (if (= dir :left)
              steps
              (- (count s) steps))
        [pre post] (split-at idx s)]
    (-> []
        (into post)
        (into pre))))

(defn char-idx [s letter]
  (some (fn [[idx c]]
          (when (= c letter)
            idx))
        (map list (range) s)))

(defmethod instruction
  :rotate-pos
  [s [_ {:keys [letter]}]]
  (let [idx (char-idx s letter)
        idx (if (<= 4 idx)
              (inc idx)
              idx)
        idx (inc idx)
        idx (mod idx (count s))]
    (->> (cycle s)
         (drop (- (count s) idx))
         (take (count s))
         vec)))

(defmethod instruction
  :reverse
  [s [_ {:keys [from to]}]]
  (let [ ;; post starts with from
        [pre post] (split-at from s)
        ;; mid ends including to
        [mid post] (split-at (- (inc to) from) post)]
    (-> []
        (into pre)
        (into (reverse mid))
        (into post))))

(defmethod instruction
  :move
  [s [_ {:keys [from to]}]]
  (cond
    (= from to)
    s
    (< from to)
    (let [ ;; post starts with from
          [pre post] (split-at from s)
          ;; mid ends including to-idx
          [mid post] (split-at (- (inc to) from) post)]
      (-> []
          (into pre)
          (into (rest mid))
          (conj (first mid))
          (into post)))
    (> from to)
    (let [ ;; post starts with from
          [pre post] (split-at from s)
          ;; mid start including to-idx
          [pre mid] (split-at to pre)]
      (-> []
          (into pre)
          (conj (first post))
          (into mid)
          (into (rest post))))))


(defmulti inv-instruction (fn [s [type args]]
                            type))

(defmethod inv-instruction
  :swap-pos
  [s [t {:keys [from to]}]]
  (instruction s [t {:from to, :to from}]))

(defmethod inv-instruction
  :swap-letter
  [s [t {:keys [from to]}]]
  (instruction s [t {:from to, :to from}]))

(defmethod inv-instruction
  :rotate-dir
  [s [t {:keys [dir steps]}]]
  (instruction s [t {:dir ({:left :right, :right :left} dir)
                     :steps steps}]))

(def get-inv-mapping
  (memoize (fn [n]
             ;; {idx {out-idx in-idx, ....}, ....}
             (into {}
                   (for [p (range n)]
                     (let [in [:rotate-pos {:letter p}]
                           s (vec (range n))
                           out (instruction s in)
                           out-idx (char-idx out p)]
                       [out-idx (zipmap out (range n))]))))))

(comment
  (get-inv-mapping 4)
  (map
   #(instruction (vec (range 8)) [:rotate-pos {:letter %}])
   (range 8))
  )

(defmethod inv-instruction
  :rotate-pos
  [s [_ {:keys [letter]}]]
  (let [idx (char-idx s letter)
        inv-map (get (get-inv-mapping (count s)) idx)]
    (mapv
     (fn [i]
       (get s (get inv-map i)))
     (range (count s)))))

(defmethod inv-instruction
  :reverse
  [s [t {:keys [from to]}]]
  (instruction s [t {:from from, :to to}]))

(defmethod inv-instruction
  :move
  [s [t {:keys [from to]}]]
  (instruction s [t {:from to, :to from}]))


(defn day21 [start ins-str]
  (let [ins (s/conform ::ins ins-str)]
    (->> (reduce
          (fn [s in]
            (let [res (instruction s in)]
              (let [inv-res (inv-instruction res in)]
                (when-not (= inv-res s)
                  (println in)
                  (println "inv-in:  " res)
                  (println "inv-res: " inv-res)
                  (println "expected:" s)
                  (println "")))
              res))
          (vec start)
          ins)
         (apply str))))

(defn day21-2 [start ins-str]
  (let [ins (s/conform ::ins ins-str)]
    (->> (reduce
          inv-instruction
          (vec start)
          (reverse ins))
         (apply str))))

(comment
  (day21 "abcde" example-in) ;;=> "decab"
  (day21 "abcdefgh" (slurp (io/resource "day21.txt"))) ;; => "aefgbcdh"
  (= (instruction '[h b c g e a f d] [:reverse {:from 6, :to 7}])
     '[h b c g e a d f])

  (= (instruction (seq "abcdefgh") [:rotate-dir {:dir :left, :steps 1}])
     (seq "bcdefgha"))
  (= (instruction (seq "abcdefgh") [:rotate-dir {:dir :right, :steps 3}])
     (seq "fghabcde"))

  (day21-2 "fbgdceah" (slurp (io/resource "day21.txt")));; => "egcdahbf"
  )
