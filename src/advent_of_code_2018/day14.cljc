(ns advent-of-code-2018.day14
  (:require [clojure.test :refer [is]]))


(defn calc
  [start n]
  (loop [elf1 0
         elf2 1
         recipes start
         length (count recipes)
         scores []]
    (if (> length n)
      {:recipes recipes :scores scores}
      (let [score1 (nth recipes elf1)
            score2 (nth recipes elf2)
            scores (conj scores (+ score1 score2))
            digits (map #(Integer/parseInt (str %)) (str (+ score1 score2)))
            new-recipes (reduce conj recipes digits)
            length (count new-recipes)
            elf1 (mod (inc (+ elf1 score1)) length)
            elf2 (mod (inc (+ elf2 score2)) length)]
        (recur elf1 elf2 new-recipes length scores)))))

(defn get-first-answer
  [input n after]
  (let [{recipes :recipes scores :scores} (calc input (+ n after))]
    (println "recipes" (apply str (take n (nthnext recipes after))))
    (println (apply str scores))))


(defn vindex-of
  {:test (fn []
           (is (= 1 (vindex-of [2 3 4] [1 2 3 4 5])))
           (is (= -1 (vindex-of [2 3 4] [3 2 4 5])))
           (is (= 0 (vindex-of [5 6 4 2] [5 6 4 2])))
           (is (= -1 (vindex-of [2 3 4] [])))
           (is (= 3 (vindex-of [2 3 4] [5 6 7 2 3 4]))))}
  [v1 v2]
  (let [len1 (count v1)]
    (loop [remaining v2
           index 0]
      (if (> len1 (count remaining))
        -1
        (let [v (take len1 remaining)]
          (if (= v v1)
            index
            (recur (rest remaining) (inc index))))))))


(defn calc2
  [start search]
  (loop [elf1 0
         elf2 1
         recipes start
         last start]
    (let [score1 (nth recipes elf1)
          score2 (nth recipes elf2)
          digits (map #(Integer/parseInt (str %)) (str (+ score1 score2)))
          new-recipes (reduce conj recipes digits)
          length (count new-recipes)
          elf1 (mod (inc (+ elf1 score1)) length)
          elf2 (mod (inc (+ elf2 score2)) length)
          last+new (reduce conj last digits)]
      ;(println "search" search "l+n" last+new)
      (let [index (vindex-of search last+new)]
        ;(println index)
        (if (>= index 0)
          (+ index (- length (count last+new)))
          (recur elf1 elf2 new-recipes (vec (take-last (count search) last+new))))))))

(defn get-second-answer
  {:test (fn []
           (is (= 9 (calc2 [3 7] [5 1 5 8 9])))
           (is (= 5 (calc2 [3 7] [0 1 2 4 5])))
           (is (= 18 (calc2 [3 7] [9 2 5 1 0])))
           (is (= 2018 (calc2 [3 7] [5 9 4 1 4]))))}
  [input search]
  (calc2 input search))
