(ns advent.day7
  (:require [clojure.spec :as s]
            [clojure.test :as test]
            [clojure.java.io :as io]))

(def t1 "abba[mnop]qrst
abcd[bddb]xyyx
aaaa[qwer]tyui
ioxxoj[asdfgh]zxcvbn
ioxxoj[asdfgh]zxcvbnioxxoj[asdfgh]zxcvbn
")

(def alphabet "abcdefghijklmnopqrstuvwxyz")
(def letter (set alphabet))

(s/def ::ips (s/and string?
                    (s/conformer seq)
                    (s/* (s/cat
                          :ip
                          (s/cat
                           :reg (s/*
                                 (s/cat :outer (s/+ letter)
                                        :inner (s/cat
                                                :openbracket #{\[}
                                                :inner (s/+ letter)
                                                :closebracket #{\]})))
                           :outer (s/? (s/+ letter)))
                          :newline #{\newline}))
                    (s/conformer
                     (fn [parsed]
                       (->> parsed
                            (map :ip)
                            (map (fn [m]
                                   {:inners (mapv (comp :inner :inner) (:reg m))
                                    :outers (-> []
                                                (conj (:outer m))
                                                (into (map :outer (:reg m))))})))))))


(s/def ::charvec (s/coll-of char? :kind vector?))
(s/def ::inners (s/coll-of ::charvec))
(s/def ::outers (s/coll-of ::charvec))

(s/def ::parsed (s/coll-of (s/keys :req-un [::inners ::outers])))

(defn abba [charvec]
  (->> charvec
       (partition 4 1)
       (some (fn [[a b c d]]
               (and (not= a b)
                    (= a d)
                    (= b c))))))

(defn valid-ip [parsed]
  (boolean
   (and (some abba (:outers parsed))
        (not (some abba (:inners parsed))))))

(defn valid-ips [str-in]
  (let [ips (s/conform ::ips str-in)
        ips (s/conform ::parsed ips)]
    (map valid-ip ips)))

(defn day7 []
  (let [in (slurp (io/resource "day7.txt"))]
    (count (filter identity (valid-ips in)))))

(comment
  (s/conform ::ips t1)
  (s/explain ::ips t1)
  (assert (= (valid-ips t1) [true false false true true]))
  (day7) ;; => 110
  )

(def t2 "aba[bab]xyz
xyx[xyx]xyx
aaa[kek]eke
zazbz[bzb]cdb
")

;; = 
(defn keep-aba [charvec]
  (let [res (->> charvec
                 (partition 3 1)
                 (keep (fn [[a b c :as in]]
                         (when (and (not= a b)
                                    (= a c))
                           in))))]
    (println charvec "-->" res)
    res))

(defn invert [[a b c :as in]]
  (when in
    [b a b]))


(defn ssl? [parsed]
  (let [aba-inverted (into #{}
                           (comp
                            (mapcat keep-aba)
                            (map invert))
                           (:outers parsed))

        #_(set (map invert (mapcat keep-aba (:outers parsed))))]
    (println "aba-i" aba-inverted)
    #_(some aba-inverted (mapcat keep-aba (:inners parsed)))
    (let [res (transduce
               (comp
                (mapcat keep-aba)
                (filter aba-inverted)
                (halt-when identity))
               identity
               nil
               (:inners parsed))]
      (println "th" (:inners parsed) "=>" res)
      res)))

(defn valid-ips2 [str-in]
  (let [ips (s/conform ::ips str-in)
        ips (s/conform ::parsed ips)]
    (map (comp boolean ssl?) ips)))

(defn day7-1 []
  (let [in (slurp (io/resource "day7.txt"))]
    (count (filter identity (valid-ips2 in)))))
(comment
  (assert (= (valid-ips2 t2) [true false true true]))
  (day7-1) ;;=> 242
  )
