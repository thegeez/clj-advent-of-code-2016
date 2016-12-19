(ns advent.day19
  (:require [clojure.data.finger-tree :as ft]))

(defn day19 [n-elves]
  (let [state (->> (zipmap (map inc (range n-elves)) ;; idx starts at 1
                           (repeat 1))
                   (into (sorted-map)))]
    (loop [state state]
      (if (= (count state) 1)
        (ffirst state)
        (recur
         (reduce
          (fn [state [to-idx from-idx]]
            (if-let [to-c (get state to-idx)]
              (-> state
                  (assoc to-idx (+ to-c (get state from-idx)))
                  (dissoc from-idx))

              ;;elf at index already had its presents stolen
              state
              ))
          state
          (->> (map first state)
               (partition 2 1
                          [(ffirst state)] ;; wrap around for last elf
                          ))))))))

(comment
  (day19 5) ;=> 3
  (day19 3014387) ;=> 1834471
  )

;; too slow
#_(defn day19-2 [n-elves]
  (let [state (mapv vector
                    (map inc (range n-elves)) ;; idx starts at 1
                    (repeat 1))]
    (loop [state state
           i 0]
      (if (= (count state) 1)
        (ffirst state)
        (let [n-elves (count state)
              [to-i to-c] (get state i)
              opp-i (mod (+ i (long (Math/floor (/ n-elves 2)))) n-elves)
              [to-i to-c] (get state i)
              [pre [[from-i from-c] & post]] (split-at opp-i state)
              state (assoc state i [to-i (+ to-c from-c)])
              state (-> []
                        (into pre)
                        (into post))
              next-i (let [ni (inc i)]
                       (if (<= (count state) i)
                         0
                         ni))]
          (recur state
                 next-i))))))
;; also slow
#_(defn day19-2 [n-elves]
  (let [state (into-array Long/TYPE (repeat n-elves 1))
        inc-i (fn [i]
                (let [ni (inc i)]
                  (if (<= n-elves i)
                    0
                    ni)))]
    (loop [state state
           i 0
           empty-c 0]
      (when (mod i 100)
        (println "i" i "empty-c" empty-c))
      (if (= empty-c (dec n-elves))
        (areduce
         state
         idx
         ret
         nil
         (or ret (aget state idx)))
        (let [elves (- n-elves empty-c)
              skip-elves (mod (int (Math/floor (/ elves 2))) n-elves)
              from-i (loop [i i
                            j skip-elves]
                       (if (zero? j)
                         i
                         (if-not (zero? (aget state i))
                           ;; saw elf
                           (recur (inc-i i) (dec j))
                           (recur (inc-i i) j)
                           )))
              to-c (aget state i)
              from-c (aget state from-i)
              _ (aset state i (+ to-c from-c))
              _ (aset state from-i 0)
              next-i (inc-i i)]
          (recur state
                 next-i
                 (inc empty-c)))))))

(comment
  (day19-2 5) ;=> 2
  (day19-2 3014387)
  )
;; too slow
#_(defn day19-2 [n-elves]
  (let [inc-i (fn [i]
                (let [ni (inc i)]
                  (if (<= n-elves i)
                    1
                    ni)))
        state (into {}
                    (comp
                     (map inc)
                     (map (fn [i]
                            [i {:gifts 1
                                :next (inc-i i)}])))
                    (range n-elves))]
    (loop [state state
           elf-i 1]
      (when (zero? (mod (count state) 1000))
        (println "state count" (count state)))
      (if (= (count state) 1)
        (ffirst state)
        (let [n-elves (count state)
              steps (mod (long (Math/floor (/ n-elves 2))) n-elves)
              pre-from-elf-i (nth (iterate (fn [i]
                                             (:next (get state i))) elf-i)
                                  (dec steps))
              from-elf-i (:next (get state pre-from-elf-i))
              from-elf (get state from-elf-i)
              {from-gifts :gifts from-next :next} from-elf
              state (-> state
                        (update-in [elf-i :gifts] + from-gifts)
                        (dissoc from-elf-i)
                        (assoc-in [pre-from-elf-i :next] from-next))]
          (recur state
                 (:next (get state elf-i))))))))

(comment
  (day19-2 5)
  (day19-2 3014387)
  )

;; wrong?
#_(defn day19-2 [n-elves]
  (let [state (into (ft/counted-double-list)
                    (comp
                     (map inc)
                     (map (fn [i]
                            [i 1])))
                    (range n-elves))]
    (loop [state state
           i 0]
;;      (println "i" i "state" state)
      (when (zero? (mod (count state) 10000))
        (println "i" i "count" (count state)))
      (if (= (count state) 1)
        (ffirst state)
        (let [in-count (count state)
              n-elves (count state)
              opp-i (mod (+ i (long (Math/floor (/ n-elves 2)))) n-elves)
              from-c (second (get state opp-i))
              #_ (assert from-c
                        (pr-str {:state state
                                 :opp-i opp-i
                                 :i i}))
              [to-i to-c] (get state i)
              #_ (assert (and to-i to-c)
                        (pr-str {:state state
                                 :i i}))
              state (assoc state i [to-i (+ to-c from-c)])
              [pre _ post] (ft/ft-split-at state opp-i)
              state (ft/ft-concat pre post)
              next-i (let [ni (if (< i opp-i)
                                (inc i)
                                i)]
                       (if (<= (count state) ni)
                         0
                         ni))]
          (assert (= (count state)
                     (dec in-count))
                  )
          (recur state
                 next-i))))))

(defn day19-2 [n-elves]
  (let [state (into (ft/counted-double-list)
                    (comp
                     (map inc)
                     (map (fn [i]
                            [i 1])))
                    (range n-elves))]
    (loop [state state]
;;      (println "i" i "state" state)
      (when (zero? (mod (count state) 10000))
        (println "count" (count state)))
      (if (= (count state) 1)
        (ffirst state)
        (let [in-count (count state)
              n-elves (count state)
              opp-i (mod (long (Math/floor (/ n-elves 2))) n-elves)
              from-c (second (get state opp-i))
              #_ (assert from-c
                        (pr-str {:state state
                                 :opp-i opp-i
                                 :i i}))
              [to-i to-c] (first state)
              #_ (assert (and to-i to-c)
                        (pr-str {:state state
                                 :i i}))
              [pre _ post] (ft/ft-split-at state opp-i)
              state (ft/ft-concat (rest pre) (conj post [to-i (+ to-c from-c)]))]
          (assert (= (count state)
                     (dec in-count))
                  )
          (recur state))))))

(comment
  (day19-2 5)
  (day19-2 3014387) ;;=> 1420064 ;;=> 31728
  )


