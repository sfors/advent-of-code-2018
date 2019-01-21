(ns advent-of-code-2018.day9
  (:require [clojure.test :refer [is]]))

(def nplayers 438)
(def highest-marble 7162600)


(defn add-at-index
  {:test (fn []
           (is (= '(1 2 3 4 7 5 6 7) (add-at-index [1 2 3 4 5 6 7] 4 7)))
           (is (= '(1 2 3 4 5 6 7 7) (add-at-index [1 2 3 4 5 6 7] 100 7)))
           (is (= '(7 1 2 3 4 5 6 7) (add-at-index [1 2 3 4 5 6 7] 0 7))))}
  [coll index x]
  (let [[start end] (split-at index coll)]
    (->> (cons x end)
         (concat start))))


(defn remove-at-index
  {:test (fn []
           (is (= '(1 2 3 4 6 7) (remove-at-index [1 2 3 4 5 6 7] 4)))
           (is (= '(1 2 3 4 5 6 7) (remove-at-index [1 2 3 4 5 6 7] 100)))
           (is (= '(2 3 4 5 6 7) (remove-at-index [1 2 3 4 5 6 7] 0))))}

  [coll index]
  (let [[start end] (split-at index coll)]
    (concat start (rest end))))


(defn get-next-player
  [current-player]
  (let [next-player (inc current-player)]
    (if (> next-player nplayers)
      1
      next-player)))


(defn get-index
  [circle index modifier]
  (mod (+ index modifier) (count circle)))


(defn get-zeroed-scoreboard
  []
  (reduce (fn [points player]
            (assoc points player 0)) {} (range 1 (inc nplayers))))


(defn play
  []
  (reduce (fn [{circle               :circle
                current-marble-index :current-marble-index
                points               :points
                player               :player}
               marble]
            (when (= 0 (mod marble 1000000)) (println marble))
            (if (= 0 (mod marble 23))
              (let [pick-up-index (get-index circle current-marble-index -7)
                    pick-up-score (nth circle pick-up-index)
                    new-circle (remove-at-index circle pick-up-index)
                    next-current-index (get-index new-circle pick-up-index 0)]
                {:current-marble-index next-current-index
                 :circle               new-circle
                 :points               (update points player + marble pick-up-score)
                 :player               (get-next-player player)})
              (let [new-marble-index (get-index circle current-marble-index 2)]
                {:circle               (add-at-index circle new-marble-index marble)
                 :current-marble-index new-marble-index
                 :points               points
                 :player               (get-next-player player)})))
          {:circle               '(0)
           :current-marble-index 0
           :points               (get-zeroed-scoreboard)
           :player               1} (range 1 (inc highest-marble))))


(defn get-first-answer
  []
  (let [game (play)]
    (apply max-key second (:points game))))


(defn get-second-answer
  []
  (let [game (play)]
    game))