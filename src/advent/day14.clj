(ns advent.day14
  (:require [clojure.spec :as s]
            [clojure.test :as test]
            [clojure.java.io :as io]
            digest))

;; digest/md5 already is lowercase everything
;;(map digest/md5 (map str (range 10)))

(defn day14 []
  (let [salt "cuanljph"
        ;;salt "abc"
        idx+keyss (reduce
                   (fn [{:keys [candidates keys]} [i md5]]
                     (let [fives (set (loop [ss md5
                                             out []]
                                        (if (nil? (first ss))
                                          out
                                          (let [[a b c d e] ss]
                                            (if (= a b c d e)
                                              (recur (drop 5 ss)
                                                     (conj out a))
                                              (recur (rest ss)
                                                     out))))))

                           [new-keys candidates] (or (when-not (seq fives)
                                                       [nil candidates])
                                                     (reduce
                                                      (fn [[nk cd] [j k :as c]]
                                                        (cond
                                                          ;; expire old candidates
                                                          (< (+ j 1000) i)
                                                          [nk cd]

                                                          (contains? fives k)
                                                          [(conj nk c) cd]

                                                          :else
                                                          [nk (conj cd c)]))
                                                      [nil []]
                                                      candidates))
                           #_ (when (seq fives)
                                (println "fives" fives "new-keys " new-keys))
                           keys (into keys new-keys)]
                       (when (seq new-keys)
                         (println "i "i " new keys" new-keys " total keys: " (count keys)))
                       (if (or #_(= i 817)
                               (>= (count keys) 64))
                         (do (println "candidates" candidates)
                             (println "keys" keys)
                             (reduced keys))
                         ;;(reduced i)
                         (let [new-candidate (loop [ss md5]
                                               (when-let [[a b c :as ss] (seq ss)]
                                                 (if (= a b c)
                                                   a
                                                   (recur (next ss)))))
                               #_ (when new-candidate
                                    (println "i" i "new-candidate" new-candidate))
                               candidates (cond-> candidates
                                            new-candidate
                                            (conj [i new-candidate]))]
                           (let [res {:candidates candidates
                                      :keys keys}]
                             #_(println "res" i md5 res)
                             res)))))
                   {:candidates []
                    :keys []}
                   (map (fn [i]
                          [i
                           #_(-> (str salt i)
                                 digest/md5)
                           ;; part 2
                           (loop [out (str salt i)
                                  i 0]
                             (let [out (digest/md5 out)]
                               (if (= i 2016)
                                 out
                                 (recur out (inc i)))))
                           ])
                        (range)))]
    (println "RES" idx+keyss)
    (nth (sort-by first idx+keyss) 63)))

