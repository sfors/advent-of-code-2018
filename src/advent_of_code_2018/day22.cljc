(ns advent-of-code-2018.day22
  (:require [clojure.test :refer [is]]))


;depth: 11109
;target: 9,731

(defn get-erosion-level
  [cave pos]
  (:el (get cave pos)))


(defn get-cave-map
  [[max-x max-y] [target-x target-y] depth]
  (loop [x 0
         y 0
         cave {}]
    (let [geo-index (cond
                      (= x y 0)
                      0
                      (and (= x target-x) (= y target-y))
                      0
                      (= 0 y)
                      (* x 16807)
                      (= 0 x)
                      (* y 48271)
                      :else
                      (* (get-erosion-level cave [(dec x) y]) (get-erosion-level cave [x (dec y)])))
          erosion-level (mod (+ geo-index depth) 20183)
          type (mod erosion-level 3)
          new-cave (assoc cave [x y] {:el erosion-level :type type})]
      (if (and (= x max-x) (= y max-y))
        new-cave
        (let [[new-x new-y] (if (>= x max-x)
                              [0 (inc y)]
                              [(inc x) y]
                              )]
          (recur new-x new-y new-cave))))))


(defn get-test-cave
  []
  (let [target-pos [10 10]
        max-pos [16 16]
        depth 510
        cave {:map     (get-cave-map max-pos target-pos depth)
              :target  target-pos
              :max-pos max-pos
              :depth   depth}]
    cave))



(defn get-first-answer
  []
  (let [[target-x target-y :as target-pos] [9 731]
        max-pos [50 1000]
        depth 11109
        cave-map (get-cave-map max-pos target-pos depth)
        
        risk (reduce-kv (fn [result [x y] v]
                          (if (and (<= x target-x) (<= y target-y))
                            (+ result (:type v))
                            result))
                        0
                        cave-map)]
    risk))


(defn get-type
  [{cave-map :map} pos]
  (:type (get cave-map pos)))


(defn within-bounds?
   {:test (fn []
           (is (within-bounds? (get-test-cave) [12 16 :climb]))
           (is (within-bounds? (get-test-cave) [9 13 :none]))
           (is (within-bounds? (get-test-cave) [3 1 :torch]))
           (is (not (within-bounds? (get-test-cave) [-4 14 :none])))
           (is (not (within-bounds? (get-test-cave) [1000 5 :torch])))
           (is (not (within-bounds? (get-test-cave) [4 -3 :climb]))))}
  [cave [x y]]
  (let [[max-x max-y] (:max-pos cave)]
    (and (>= max-x x) (>= max-y y) (<= 0 x) (<= 0 y))))


(defn tool-valid?
  {:test (fn []
           (is (tool-valid? (get-test-cave) [12 16 :climb]))
           (is (tool-valid? (get-test-cave) [9 13 :none]))
           (is (tool-valid? (get-test-cave) [3 1 :torch]))
           (is (not (tool-valid? (get-test-cave) [12 16 :none])))
           (is (not (tool-valid? (get-test-cave) [9 13 :torch])))
           (is (not (tool-valid? (get-test-cave) [3 1 :climb]))))}
  [cave [x y tool]]
  (let [type (get-type cave [x y])]
    (some? (some #{tool} ({0 [:climb :torch]
                           1 [:climb :none]
                           2 [:torch :none]} type)))))


(defn get-other-tool
  [cave [x y tool]]
  (let [type (get-type cave [x y])]
    ({[0 :climb] :torch
      [0 :torch] :climb
      [1 :climb] :none
      [1 :none]  :climb
      [2 :torch] :none
      [2 :none]  :torch} [type tool])))


(defn pos-valid?
  [cave pos]
  (and (within-bounds? cave pos)
       (tool-valid? cave pos)))


(defn get-adjacent-available-positions
  [cave [x y tool] time]
  (let [up {:to [x (dec y) tool] :time (inc time)}
        down {:to [x (inc y) tool] :time (inc time)}
        left {:to [(dec x) y tool] :time (inc time)}
        right {:to [(inc x) y tool] :time (inc time)}
        new-tool {:to [x y (get-other-tool cave [x y tool])] :time (+ time 7)}
        moves [up down left right new-tool]
        valid-moves (filter (fn [{new-pos :to}]
                              (pos-valid? cave new-pos)) moves)]
    valid-moves
    ))


(defn get-reachable-positions
  [cave]
  (let [start-pos [0 0 :torch]]
    (loop [visited {start-pos {:time 0}}
           to-explore []
           {time :time current-pos :pos} {:pos start-pos :time 0}]
      ;(println "visited" visited)
      ;(println "to-exp" to-explore)
      ;(println "distance" distance "current-pos" current-pos)
      (let [available-moves (get-adjacent-available-positions cave current-pos time)
            ;moves-in-fighting-pos (filter #(in? fighting-positions (:pos %)) available-moves)
            new-moves (reduce (fn [result move]
                                (let [vis (get visited (:to move))]
                                  (if (or (nil? vis) (> (:time vis) (:time move))) 
                                    (conj result (assoc move :from current-pos))
                                    result)))
                              []
                              available-moves)
            
            new-visited (reduce (fn [result move]
                                  (assoc result (:to move) (dissoc move :to))) visited new-moves)
            
            new-to-explore (reduce (fn [result {to :to time :time}]
                                     (conj result {:pos to :time time})) to-explore new-moves)]
        ;(println "current" current-pos "available moves" available-moves "new-moves" new-moves)
        ;(println "explore" new-to-explore)
        ;(println "new-visited" new-visited)
        (println (count to-explore) (count visited))
        (if (empty? new-to-explore)
          new-visited
          (recur new-visited (vec (rest new-to-explore)) (first new-to-explore)))))))


(defn get-second-answer
  []
  (let [target-pos [9 731]
        max-pos [20 800]
        depth 11109
        cave {:map     (get-cave-map max-pos target-pos depth)
              :target  target-pos
              :max-pos max-pos
              :depth   depth}
        reachable (get-reachable-positions cave)]
    (get reachable (conj target-pos :torch))))
