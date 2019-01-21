(ns advent-of-code-2018.day8
  (:require [clojure.string :as str]
            [clojure.test :refer [is]]))


(defn read-input
  [mode]
  (let [filename (if (= mode :test) "input-08-2.txt" "input-08.txt")]
    (as-> (slurp (str "resources/input/" filename)) $
          (str/trim $)
          (str/split $ #" ")
          (map read-string $))))


(defn get-tree
  [input]
  (let [number-of-children (first input)
        data-length (second input)
        {children :children new-input :input} (reduce
                                                (fn [{children :children input :input} _]
                                                  (let [{child :tree remaining-input :remaining-input} (get-tree input)]
                                                    {:children (conj children child)
                                                     :input    remaining-input}))
                                                {:children [] :input (nthnext input 2)}
                                                (range number-of-children))
        data (map (fn [index]
                    (nth new-input index)) (range data-length))
        output
        {:tree            {:number-of-children number-of-children
                           :data-length        data-length
                           :data               data
                           :children           children}
         :remaining-input (nthnext new-input data-length)}]
    output
    ))


(defn sum-data
  [{children :children data :data}]
  (let [children-sums (map sum-data children)
        total-sum (+ (reduce + children-sums) (reduce + data))]
    total-sum))


(defn get-value
  [{children :children data :data}]
  (if (empty? children)
    (reduce + data)
    (reduce (fn [sum index]
              (if (and (> index 0) (< (dec index) (count children)))
                (+ sum (get-value (nth children (dec index))))
                sum)) 0 data)))


(defn get-first-answer
  [mode]
  (-> (read-input mode)
      (get-tree)
      :tree
      (sum-data)))


(defn get-second-answer
  [mode]
  (-> (read-input mode)
      (get-tree)
      :tree
      (get-value)))
