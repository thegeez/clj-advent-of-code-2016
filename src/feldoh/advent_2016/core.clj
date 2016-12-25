(ns feldoh.advent-2016.core
  (:use clojure.test))

(defn get-input-lines [filename] (clojure.string/split-lines
             (slurp (clojure.java.io/resource filename))))

(defn parseInt "Wrapper for Java Integer.parseInt"
  [arg] (Integer/valueOf arg))

(with-test
  (defn transpose [vectors] (apply map vector vectors))
  (is (= (transpose ["abc" "def" "ghi"]) [[\a \d \g] [\b \e \h] [\c \f \i]]))
  (is (= (transpose ["abcx" "defy" "ghiz"]) [[\a \d \g] [\b \e \h] [\c \f \i] [\x \y \z]])))

(run-tests)
