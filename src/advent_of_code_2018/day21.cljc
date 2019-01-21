(ns advent-of-code-2018.day21
  (:require [clojure.string :as str]
            [clojure.test :refer [is]]
            [advent-of-code-2018.day19 :refer [read-program run-program]]))


(defn get-first-answer
  []
  (let [program (read-program "resources/input/input-21.txt")
        start-register-list (list [0 0 0 0 0 0]
                                  [1 0 0 0 0 0]
                                  [2 0 0 0 0 0]
                                  [3 0 0 0 0 0]
                                  [4 0 0 0 0 0]
                                  [5 0 0 0 0 0]
                                  [6 0 0 0 0 0]
                                  [7 0 0 0 0 0]
                                  [8 0 0 0 0 0]
                                  [9 0 0 0 0 0])]
    (pmap (fn [start-registers]
            (run-program program start-registers)) start-register-list)))



(defn get-answer
  []
  (let [program (read-program "resources/input/input-21.txt")
        start-register [0 0 0 0 0 0]]
    (run-program program start-register)))

(defn get-answer2
  []
   (let [program (read-program "resources/input/input-21.txt")]
     (->> (range 0 10000)
          (pmap (fn [x]
                  (let [result (run-program program [x 0 0 0 0 0])]
                    {:start x :result result})))
          (filter (fn [run]
                    (= :normal (get-in run [:result :status])))))))
