(ns advent-of-code-2018.day5
  (:require [clojure.string :as str]
            [clojure.test :refer [is]]))


(defn read-input
  []
  (->
    (slurp "resources/input/input-05.txt")
    (str/trim-newline)))


(def all-types
  "abcdefghijklmnopqrstuvxyz")


(defn is-uppercase?
  [character]
  (Character/isUpperCase character))


(defn get-polarity
  {:test (fn []
           (is (= :big (get-polarity \V)))
           (is (= :small (get-polarity \x))))}
  [unit]
  (if (is-uppercase? unit)
    :big
    :small))


(defn different-polarity?
  {:test (fn []
           (is (different-polarity? \e \E))
           (is (not (different-polarity? \y \y)))
           (is (not (different-polarity? \T \T))))}
  [u1 u2]
  (not (= (get-polarity u1) (get-polarity u2))))

(defn same-type?
  {:test (fn []
           (is (same-type? \e \E))
           (is (same-type? \y \y))
           (is (same-type? \G \G))
           (is (not (same-type? \t \L)))
           (is (not (same-type? \d \h)))
           (is (not (same-type? \W \B))))}
  [u1 u2]
  (= (Character/toLowerCase u1) (Character/toLowerCase u2)))


(defn react?
  {:test (fn []
           (is (react? \u \U))
           (is (react? \W \w))
           (is (not (react? \C \x)))
           (is (not (react? \c \x))))}
  [u1 u2]
  (and (same-type? u1 u2) (different-polarity? u1 u2)))


(defn react
  [in-polymer]
  (loop [polymer in-polymer
         done []]
    (if (empty? polymer)
      done
      (let [unit (first polymer)
            next-unit (second polymer)]
        (if (nil? next-unit)
          (conj done unit)
          (if (react? unit next-unit)
            (recur (rest (rest polymer)) done)
            (recur (rest polymer) (conj done unit))))))))


(defn perform-reactions-until-dead
  [polymer]
  (loop [polymer polymer]
    (let [new-polymer (react polymer)
          reaction-has-happened (> (count polymer) (count new-polymer))]
      (if reaction-has-happened
        (recur new-polymer)
        polymer))))


(defn get-first-answer
  []
  (->> (read-input)
       (perform-reactions-until-dead)
       (apply str)
       (count)))

(defn get-second-answer
  []
  (->>
    (let [base-polymer (read-input)]
      (pmap (fn [type]
              (let [filtered-polymer (filter #(not (= type (Character/toLowerCase %))) base-polymer)
                    reacted-polymer (perform-reactions-until-dead filtered-polymer)]
                {:type    type
                 :polymer reacted-polymer
                 :size    (count reacted-polymer)}))
            all-types))
    (sort-by :size)
    (first)))
