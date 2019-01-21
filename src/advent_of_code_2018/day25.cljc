(ns advent-of-code-2018.day25
  (:require [clojure.string :as str]
            [clojure.test :refer [is]]
            [clojure.set :refer [union difference]]))

(defn read-input
  [mode]
  (let [filename (if (= mode :test) "input-25-2.txt" "input-25.txt")]
    (->> (slurp (str "resources/input/" filename))
         (str/split-lines)
         (map (fn [line]
                (let [strings (str/split line #",")]
                  (->> strings
                       (map #(Integer/parseInt %))
                       (vec))))))))


(defn get-test-points
  []
  (list [0, 0, 0, 0]
        [3, 0, 0, 0]
        [0, 3, 0, 0]
        [0, 0, 3, 0]
        [0, 0, 0, 3]
        [0, 0, 0, 6]
        [9, 0, 0, 0]
        [12, 0, 0, 0]))


(defn abs
  {:test (fn []
           (is (= 5 (abs 5)))
           (is (= 5 (abs -5))))}
  [x]
  (if (> 0 x) (- x) x))


(defn distance
  {:test (fn []
           (is (= 8 (distance [1 1 1 1] [3 3 3 3]))))}
  [[x1 y1 z1 t1] [x2 y2 z2 t2]]
  (+ (abs (- x2 x1)) (abs (- y2 y1)) (abs (- z2 z1)) (abs (- t2 t1))))


(defn connected?
  {:test (fn []
           (is (connected? [0 0 0 0] [0 0 0 3]))
           (is (connected? [3 2 0 1] [2 2 0 3]))
           (is (not (connected? [6 0 0 0] [0 0 0 0])))
           )}
  [pos1 pos2]
  (>= 3 (distance pos1 pos2)))


(defn get-connections
  [positions]
  (reduce (fn [result pos]
            (let [connections (->> positions
                                   (filter #(connected? pos %))
                                   (remove #(= % pos))
                                   (set))]
              (assoc result pos connections)))
          {}
          positions))


(defn get-first-unvisited-position
  {:test (fn []
           (is (= (get-first-unvisited-position
                    [[1 2] [2 4] [3 7]]
                    [[2 4] [1 2]])
                  [3 7])))}
  [positions visited]
  (some (fn [pos]
          (when (not-any? #(= pos %) visited)
            pos)) positions))


(defn get-constellations
  {:test (fn []
           (let [connections (get-connections (get-test-points))
                 constellations (get-constellations connections)]
             (is (contains? constellations #{[0, 0, 0, 0]
                                             [3, 0, 0, 0]
                                             [0, 3, 0, 0]
                                             [0, 0, 3, 0]
                                             [0, 0, 0, 3]
                                             [0, 0, 0, 6]}))
             (is (contains? constellations #{[9, 0, 0, 0]
                                             [12, 0, 0, 0]}))
             (is (= 2 (count constellations)))))}
  [connections]
  (let [points (set (map key connections))]
    (loop [current (first points)
           visited #{}
           to-explore #{}
           current-constellation #{}
           constellations #{}]
      (let [connected (connections current)
            new-to-explore (union to-explore (difference connected visited))
            new-visited (conj visited current)
            new-current-constellation (conj current-constellation current)]
        (if (empty? new-to-explore)
          (let [next-point (first (difference points new-visited))
                new-constellations (conj constellations new-current-constellation)]
            (if (nil? next-point)
              new-constellations
              (recur next-point new-visited #{} #{} new-constellations)))
          (let [next (first new-to-explore)]
            (recur next
                   new-visited
                   (disj new-to-explore next)
                   new-current-constellation
                   constellations)))))))


(defn get-first-answer
  [mode]
  (->> (read-input mode)
       (get-connections)
       (get-constellations)
       (count)))