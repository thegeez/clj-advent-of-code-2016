(ns advent.day16)

(defn to-ints [i]
  (mapv {\1 1 \0 0} (seq (str i))))

(comment
  (rseq (to-ints 10000))
  )

(defn next-data [is]
  (-> is
      (conj 0)
      (into (map {1 0, 0 1} (rseq is)))))

(defn checksum [is]
  (->> is
       (partition 2)
       (map {[0 0] 1
             [0 1] 0
             [1 0] 0
             [1 1] 1})))

(defn day16 []
  (let [;;init 10000
        ;;min-length 20
        init 11011110011011101
        ;;min-length 272
        min-length 35651584
        data (->> init
                  to-ints
                  (iterate next-data)
                  (some (fn [is]
                          (when (<= min-length (count is))
                            is)))
                  (take min-length)
                  (iterate checksum)
                  (some (fn [cs]
                          (when (odd? (count cs))
                            cs))))]
    (apply str data)))

(defn day16-2 []
  (let [;;init 10000
        ;;min-length 20
        init 11011110011011101
        ;;min-length 272
        min-length 35651584
        data (loop [is (to-ints init)]
               (if (<= min-length (count is))
                 is
                 (recur (into (conj is 0)
                              (comp (map {1 0, 0 1}))
                              (rseq is)))))
        data (into [] (take min-length) data)
        checksum (loop [cs data]
                   (let [cs (loop [[a b & cs] cs
                                   out []]
                              ;; always even count in cs
                              (if (nil? a)
                                out
                                (recur cs
                                       (conj out (if (= a b)
                                                   1
                                                   0)))))]
                     (if (odd? (count cs))
                       cs
                       (recur cs))))]
    (apply str checksum)))

(comment
  (day16) ;; => "00000100100001100"
  (day16-2); => "00011010100010010"
  )
