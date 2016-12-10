(ns advent.day4
  (:require [clojure.spec :as s]
            [clojure.test :as test]
            [clojure.java.io :as io]))

(def t1
  "aaaaa-bbb-z-y-x-123[abxyz]
a-b-c-d-e-f-g-h-987[abcde]
not-a-real-room-404[oarel]
totally-real-room-200[decoy]
")

(def nums (set "0123456789"))
(def alphabet "abcdefghijklmnopqrstuvwxyz")
(def letters (set alphabet))

(s/def ::lines (s/and string?
                      (s/conformer seq)
                      (s/+ (s/cat :chars (s/+ (s/cat :chars (s/+ letters)
                                                     :sep #{\-}))
                                  :id (s/+ nums)
                                  :open #{\[}
                                  :checksum (s/& (s/+ letters)
                                                 #(= (count %) 5))
                                  :close #{\]}
                                  :newline #{\newline}
                                  ))
                      (s/conformer
                       (fn [res]
                         (mapv #(-> %
                                    (select-keys [:chars :id :checksum])
                                    (update-in [:chars] (fn [cs]
                                                          (mapcat :chars cs)))
                                    (update-in [:id] (fn [ns]
                                                       (Long/parseLong (apply str ns)))))
                               res)))))

(def char-index (zipmap alphabet (range)))
(defn char> [c1 c2]
  (< (char-index c1)
     (char-index c2)))

(defn valid-room [{:keys [chars checksum] :as room}]
  (let [calced (->> chars
                    frequencies
                    (sort-by
                     identity
                     (fn [[c1 n1] [c2 n2]]
                       (or (> n1 n2)
                           (and (= n1 n2)
                                (char> c1 c2)))))
                    (take 5)
                    (map first))]
    (= calced checksum)))

(defn check [str-in]
  (let [rooms (s/conform ::lines str-in)]
    (filter valid-room rooms)
    (transduce
     (comp
      (filter valid-room)
      (map :id))
     +
     rooms)))

(defn day4 []
  (let [in (slurp (io/resource "day4.txt"))]
    (check in)))

(s/def ::lines2 (s/and string?
                      (s/conformer seq)
                      (s/+ (s/cat :chars (s/+ (into letters #{\-}))
                                  :id (s/+ nums)
                                  :open #{\[}
                                  :checksum (s/& (s/+ letters)
                                                 #(= (count %) 5))
                                  :close #{\]}
                                  :newline #{\newline}
                                  ))
                      (s/conformer
                       (fn [res]
                         (mapv #(-> %
                                    (select-keys [:chars :id :checksum])
                                    (update-in [:id] (fn [ns]
                                                       (Long/parseLong (apply str ns)))))
                               res)))))

(defn decode [{:keys [chars id] :as entry}]
  (let [decoding (-> (zipmap alphabet
                             (take (count alphabet)
                                   (drop id (cycle alphabet))))
                     (merge {\- \space}))
        decoded (transduce
                 (replace decoding)
                 conj
                 chars)
        decoded (apply str decoded)]
    (assoc entry :decoded decoded)))

(defn day4-1 []
  (let [in (slurp (io/resource "day4.txt"))
        rooms (s/conform ::lines2 in)]
    (println (take 4 rooms))
    (->> rooms
         (map decode)
         ((fn [res]
            (doseq [r res]
              (println (:decoded r)))
            res))
         (some #(when (= (:decoded %) "northpole object storage ")
                  %))
         #_:id)))

(comment
  (s/conform ::lines t1)
  (check t1) ;; 1514

  (day4) ;; 245102
  #_[{:chars (\a \a \a \a \a \b \b \b \z \y \x), :id 123, :checksum [\a \b \x \y \z]} {:chars (\a \b \c \d \e \f \g \h), :id 987, :checksum [\a \b \c \d \e]} {:chars (\n \o \t \a \r \e \a \l \r \o \o \m), :id 404, :checksum [\o \a \r \e \l]} {:chars (\t \o \t \a \l \l \y \r \e \a \l \r \o \o \m), :id 200, :checksum [\d \e \c \o \y]}]
  (s/explain ::lines t1)

  (s/conform ::lines2 t1)
  (s/explain ::lines2 t1)
  (day4-1) ;;{:chars [\b \c \f \h \v \d \c \z \s \- \c \p \x \s \q \h \- \g \h \c \f \o \u \s \-], :id 324, :checksum [\c \h \s \f \b], :decoded "northpole object storage "}
  )
