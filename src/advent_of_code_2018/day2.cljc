(ns advent-of-code-2018.day2
  (:require [clojure.test :refer [is]])
  )

(defn read-input
  []
  (->> (slurp "resources/input/input-02.txt")
       (clojure.string/split-lines)))


(defn contains-twos?
  {:test (fn []
           (is (not (contains-twos? "abcdef")))
           (is (contains-twos? "bababc"))
           (is (contains-twos? "aabcdd")))}
  [id]
  (some (fn [[_ n]] (= 2 n)) (frequencies id)))


(defn contains-threes?
  {:test (fn []
           (is (not (contains-threes? "abcdef")))
           (is (contains-threes? "bababc"))
           (is (contains-threes? "abcccd")))}
  [id]
  (some (fn [[_ n]] (= 3 n)) (frequencies id)))


(defn get-first-answer
  []
  (let [result (reduce
                 (fn [{twos :twos threes :threes}
                      id]
                   {:twos   (if (contains-twos? id) (inc twos) twos)
                    :threes (if (contains-threes? id) (inc threes) threes)})
                 {:twos 0 :threes 0}
                 (read-input))]
    (* (:twos result) (:threes result))))


(defn distance
  {:test (fn []
           (is (= 2 (distance "abcde" "axcye"))))}
  [id1 id2]
  (->> (map = id1 id2)
       (filter false?)
       (count)))


(defn common-letters
  [id1 id2]
  (->> (map (fn [l1 l2] (if (= l1 l2) l1)) id1 id2)
       (filter some?)
       (apply str)))


(defn get-second-answer
  []
  (let [input (read-input)]
    (->>
      (loop [id1 (first input)
             ids (rest input)]
        (let [match (some (fn [id2]
                            (if (= (distance id1 id2) 1) id2))
                          input)]
          (if (some? match)
            [id1 match]
            (recur (first ids) (rest ids)))))
      (apply common-letters))))
