(ns net.thegeez.advent.spec-parsing
  (:require [clojure.string :as str]
            [clojure.spec :as s]
            [clojure.spec.gen :as gen]
            [clojure.test.check.generators :as tgen]))
;; Dependencies:
;; [org.clojure/clojure "1.9.0-alpha14"]
;; [org.clojure/test.check "0.9.0"]

;; Advent of Code is a series of code challenges in the form of an advent calender, counting down to Christmas. The challenges are given as a list of inputs and the solution needs to be entered in a text box on the website. So you can use any programming language you like!

(def fun "http://adventofcode.com/")

;; Clojure 1.9 includes clojure.spec which can do parsing with regular expressions. This can be used to parse the input for the challenges for Advent of Code!

;; The challenge for day 7 is to find valid ip addresses that are defined by some property that make them proper ip addresses for the Easter Bunny headquarters.

(def example-ips
   "abba[mnop]qrst
abcd[bddb]xyyx
aaaa[qwer]tyui
ioxxoj[asdfgh]zxcvbn
ioxxoj[asdfgh]zxcvbnioxxoj[asdfgh]zxcvbn
")
;;
;; The first 4 lines contain example ips, the last one is more like the ones found in the actual input for the challenge. The challenge is to find only the ips that have some property for the characters outside the square brackets and the characters within the quare brackets. So to solve this challenge it would be usefull to turn a string like "abba[mnop]qrst" into {:outers [[\a \b \b \a] [\q \r \s \t]] :inners [[\m \n \o \p]]}. Clojure.spec can help with that.

;; Remember that strings is Clojure are also sequences of characters:
(= (seq "abcd") [\a \b \c \d]) ;;=> true

;; Some helpers:
(def alphabet "abcdefghijklmnopqrstuvwxyz")
(def letter (set alphabet))

;; Parsing a string with clojure.spec:
(s/conform (s/* letter) "abcd") ;;=>  :clojure.spec/invalid, because the regex spec works on sequences
(s/conform (s/* letter) (seq "abcd")) ;;=> [\a \b \c \d]
;; Or
(s/conform (s/and string?
                  (s/conformer seq)
                  (s/* letter))
           "abcd") ;;=> [\a \b \c \d]
;; Note that because string? has been matched already, the conformer `seq` will never fail here, so the conformer can never return :clojure.spec/invalid

;; Lets first parse the input into an ip per line
(s/def ::ips (s/and string?
                    (s/conformer seq)
                    (s/+ (s/cat
                          :ip (s/+ (into letter "[]"))
                          :newline #{\newline}))))

(s/conform ::ips example-ips)
;;=>
;; [{:ip [\a \b \b \a \[ \m \n \o \p \] \q \r \s \t], :newline \newline}
;;  {:ip [\a \b \c \d \[ \b \d \d \b \] \x \y \y \x], :newline \newline}
;; .. etc ..]

;; Lets specify the definition of an ip a bit further
(s/def ::ips (s/and string?
                    (s/conformer seq)
                    (s/+ (s/cat
                          :ip ::ip
                          :newline #{\newline}))))

(s/def ::ip (s/cat :outer-first (s/+ letter)
                   :inner (s/cat
                           :openbracket #{\[}
                           :inner (s/+ letter)
                           :closebracket #{\]})
                   :outer-last (s/+ letter)))
(s/conform ::ips example-ips) ;;=> :clojure.spec/invalid
(s/explain-data ::ips example-ips)
;; => #:clojure.spec{:problems [{:path [:newline], :pred #{\newline}, :val \[, :via [:net.thegeez.advent.spec-parsing/ips], :in [92]}]}

(subs example-ips 92) ;;=> "[asdfgh]zxcvbn\n"
;; This shows that the ::ip spec works for the simple examples, but fails to parse the second inner part from the last example, which is more like the real input for the challenge

;; One attempt can be to look for a repeating pattern of outer parts such as "abcd" followed by an inner part "[xyz]" where the inner part is optional, because there is now inner part at the end of an ip.
(s/def ::ip (s/* (s/cat :outer (s/+ letter)
                        :inner (s/?
                                (s/cat
                                 :openbracket #{\[}
                                 :inner (s/+ letter)
                                 :closebracket #{\]})))))

;; (s/conform ::ips example-ips) ;; WARNING! running this will be very very very slow!

;; This is not a good idea, because we run into a common regular expression problem. Because the inner part is defined as optional, there are many ways to parse an ip now. We are looking for the first part to be parsed as [{:outer [\a \b \c \d] :inner ..etc..}]. Because the inner part is optional, it can also be parsed as [{:outer [\a]} {:outer [\b]} .. etc ..]. The regular expression can use some help with the parsing.

(s/def ::ip (s/cat :beginning
                   (s/* (s/cat :outer (s/+ letter)
                               :inner (s/cat
                                       :openbracket #{\[}
                                       :inner (s/+ letter)
                                       :closebracket #{\]})))
                   :end (s/+ letter)))

(s/conform ::ips example-ips)q
;; => [{:ip {:beginning [{:outer [\a \b \b \a], :inner {:openbracket \[, :inner [\m \n \o \p], :closebracket \]}}], :end [\q \r \s \t]}, :newline \newline}
;;      ... etc ...
;;     {:ip {:beginning [{:outer [\i \o \x \x \o \j], :inner {:openbracket \[, :inner [\a \s \d \f \g \h], :closebracket \]}} {:outer [\z \x \c \v \b \n \i \o \x \x \o \j], :inner {:openbracket \[, :inner [\a \s \d \f \g \h], :closebracket \]}}], :end [\z \x \c \v \b \n]}, :newline \newline}]

;; By rewriting the regular expression without using the optional predicate (s/? ..) the ips can be parsed again.

;; Sadly, now the structure of the conformed values is a bit nested, while we only care about the :outer and :inner parts of and ip. With a custom conformer this can be transformed into a shape that is easier to use
(s/def ::ip
  (s/& (s/cat :beginning
              (s/* (s/cat :outer (s/+ letter)
                          :inner (s/cat
                                  :openbracket #{\[}
                                  :inner (s/+ letter)
                                  :closebracket #{\]})))
              :end (s/+ letter))
       (s/conformer
        (fn [parsed]
          {:inners (mapv (comp :inner :inner) (:beginning parsed))
           :outers (-> []
                       (into (map :outer (:beginning parsed)))
                       (conj (:end parsed)))}))))

(s/conform ::ips example-ips)
;; => [{:ip {:inners [[\m \n \o \p]], :outers [[\a \b \b \a] [\q \r \s \t]]}, :newline \newline}
;;     {:ip {:inners [[\b \d \d \b]], :outers [[\a \b \c \d] [\x \y \y \x]]}, :newline \newline}
;;     {:ip {:inners [[\q \w \e \r]], :outers [[\a \a \a \a] [\t \y \u \i]]}, :newline \newline}
;;     {:ip {:inners [[\a \s \d \f \g \h]], :outers [[\i \o \x \x \o \j] [\z \x \c \v \b \n]]}, :newline \newline}
;;     {:ip {:inners [[\a \s \d \f \g \h] [\a \s \d \f \g \h]], :outers [[\i \o \x \x \o \j] [\z \x \c \v \b \n \i \o \x \x \o \j] [\z \x \c \v \b \n]]}, :newline \newline}]

;; And now the input is in a nice shape to solve the actual challenge with.

;; Clojure specs can work both ways. From a conformed parsing result you can go back to the original input:
(s/unform ::ip {:inners [[\m \n \o \p]], :outers [[\a \b \b \a] [\q \r \s \t]]})
;; => Throws Exception: IllegalStateException no unform fn for conformer  clojure.spec/spec-impl/reify--13832 (spec.clj:870)

;; But that does require that every step is defined to both conform and unform. In the ::ip example there is a conformer, but not an unformer that works the other way. Lets add that one:
(s/def ::ip
  (s/& (s/cat :beginning
              (s/* (s/cat :outer (s/+ letter)
                          :inner (s/cat
                                  :openbracket #{\[}
                                  :inner (s/+ letter)
                                  :closebracket #{\]})))
              :end (s/+ letter))
       (s/conformer
        (fn [parsed]
          {:inners (mapv
                    (comp :inner :inner) (:beginning parsed))
           :outers (-> []
                       (into (map :outer (:beginning parsed)))
                       (conj (:end parsed)))})
        (fn [out]
          {:beginning (for [[outer inner] (->> (interleave (butlast (:outers out)) (:inners out))
                                               (partition 2))]
                        {:outer outer
                         :inner {:openbracket \[
                                 :inner inner
                                 :closebracket \]}})
           :end (last (:outers out))}))))

(s/unform ::ip {:inners [[\m \n \o \p]], :outers [[\a \b \b \a] [\q \r \s \t]]})
;; => (\a \b \b \a \[ \m \n \o \p \] \q \r \s \t)

;; And adding an unformer to the ::ips spec:
(s/def ::ips (s/and string?
                    (s/conformer seq  #(apply str %))
                    (s/+ (s/cat
                          :ip ::ip
                          :newline #{\newline}))))
(s/unform ::ips [{:ip {:inners [[\m \n \o \p]], :outers [[\a \b \b \a] [\q \r \s \t]]}}]) ;; => "abba[mnop]qrst"

(= (s/unform ::ips (s/conform ::ips example-ips)) example-ips) ;; => true!

;; With specs you can also generate data: 
(gen/generate (s/gen ::ip)) ;;=> (\h \r \e \h  \[ \v \r \x \g \x \] \k \q \v \y \o \i \y \g \i \s \n \j ... a probably very long generated sample ...)

;; You can also generate your own input for the Advent of Code challenge:
(gen/generate (s/gen ::ips)) ;; => Exception "Couldn't satisfy such-that predicate after 100 tries."

;; What is happening here is that (s/gen ::ips) will generate random strings and not strings that conform to our ::ip spec, because (s/and ...) will use the first spec as its generator, our conformer and :ip + :newline definition are not used. This can be fixed:

(s/def ::many-ips (s/+ (s/cat
                        :ip ::ip
                        :newline #{\newline})))
(s/def ::ips (s/with-gen
               (s/and string?
                      (s/conformer seq  #(apply str %))
                      ::many-ips)
               (fn []
                 (gen/fmap
                  (fn [char-seq]
                    (apply str char-seq))
                  (s/gen ::many-ips)))))

;; now this works:
(gen/generate (s/gen ::ips)) ;;=> "btpmlpdo[nhchrwg]anbqb[qrnb]ico\njwgrxdb[vbo]grqy.... a very long string of long ips .... "



;; The challenge for day 8 in the Advent of Code is to use some instruction for a LED screen to find a password. Again the input is a string with on each line some useful input:
(def example-instructions
  "rect 31x24
rotate column x=11 by 1
rotate row y=23 by 30
rotate column x=1 by 1
")

;; To use these actions it would be helpful to have them in the shape of [[:rect {:wide 31 :tall 24}] ...etc...]

;; Some helpers again:
(def numbers "0123456789")
(def number (set numbers))
(def alphabet "abcdefghijklmnopqrstuvwxyz")
(def letter (set alphabet))

(s/def ::action (s/alt :rect (s/cat :char-r #{\r}
                                    :char-e #{\e}
                                    :char-c #{\c}
                                    :char-t #{\t}
                                    :space #{\space}
                                    :wide (s/+ number)
                                    :char-x #{\x}
                                    :tall (s/+ number))))
(s/conform ::action (seq "rect 31x24"))
;;=> [:rect {:char-r \r, :char-e \e, :char-c \c, :char-t \t, :space \space, :wide [\3 \1], :char-x \x, :tall [\2 \4]}]

;; Two things are not so nice about this. It is a bit tedious to spell out all the characters of the "rect " string and it would be nice if the numbers for :wide and :tall were actual numbers.
;; To make :wide and :tall a number:

(s/def ::number (s/with-gen
                  (s/& (s/+ number)
                       (s/conformer
                        (fn [parsed]
                          (Long/parseLong (apply str parsed)))
                        (fn [out]
                          (seq (str out)))))
                  (fn []
                    (gen/fmap
                     (fn [l]
                       (seq (str l)))
                     tgen/pos-int))))
(s/def ::action (s/alt :rect (s/cat :char-r #{\r}
                                    :char-e #{\e}
                                    :char-c #{\c}
                                    :char-t #{\t}
                                    :space #{\space}
                                    :wide ::number
                                    :char-x #{\x}
                                    :tall ::number)))

(s/conform ::action (seq "rect 31x24"))
;;=> [:rect {:char-r \r, :char-e \e, :char-c \c, :char-t \t, :space \space, :wide 31, :char-x \x, :tall 24}]
(let [in (seq "rect 31x24")]
  (= (s/unform ::action (s/conform ::action in)) in)) ;;=> true!

;; The tedious specification of string matching can be solved with a helper:
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

(s/def ::action (s/alt :rect (s/cat :text (match-seq "rect ")
                                    :wide ::number
                                    :x #{\x}
                                    :tall ::number)
                       :column (s/cat :text (match-seq "rotate column x=")
                                      :index ::number
                                      :by (match-seq " by ")
                                      :shift ::number)
                       :row (s/cat :text (match-seq "rotate row y=")
                                   :index ::number
                                   :by (match-seq " by ")
                                   :shift ::number)))
(s/def ::action-many (s/* (s/cat
                           :action ::action
                           :newline #{\newline})))
(s/def ::instructions (s/with-gen
                        (s/and string?
                               (s/conformer seq)
                               ::action-many)
                        (fn []
                          (gen/fmap
                           (fn [char-seq]
                             (apply str char-seq))
                           (s/gen ::action-many)))))

(s/conform ::instructions example-instructions)
;;=> [{:action [:rect {:text [\r \e \c \t \space], :wide 31, :x \x, :tall 24}], :newline \newline}
;;    {:action [:column {:text [\r \o \t \a \t \e \space \c \o \l \u \m \n \space \x \=], :index 11, :by [\space \b \y \space], :shift 1}], :newline \newline}
;;    {:action [:row {:text [\r \o \t \a \t \e \space \r \o \w \space \y \=], :index 23, :by [\space \b \y \space], :shift 30}], :newline \newline}
;;    {:action [:column {:text [\r \o \t \a \t \e \space \c \o \l \u \m \n \space \x \=], :index 1, :by [\space \b \y \space], :shift 1}], :newline \newline}]

;; And again you can also generate some instructions from the specs:
(gen/generate (s/gen ::instructions))
;;=> "rect 10x23\nrotate column x=11 by 8\nrotate column x=16 by 7\nrect 28x10\nrotate row y=8 by 20\nrotate column x=8 by 21\nrotate column x=19 by 27\nrect 4x0\nrotate column x=18 by 18\nrotate row y=27 by 13\nrect 10x21\nrotate column x=0 by 17\nrect 5x3\nrotate row y=13 by 14\nrotate row y=17 by 29\nrotate row y=5 by 17\nrect 25x18\nrect 20x4\nrotate column x=8 by 25\nrect 1x28\nrotate column x=3 by 4\nrect 1x24\nrect 28x3\n"
