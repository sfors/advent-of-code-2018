(ns advent-of-code-2018.day6
  (:require [clojure.string :as str]
            [clojure.test :refer [is]]))

(defn parse-int
  [s]
  (Integer/parseInt s))


(defn read-input
  []
  (->> (slurp "resources/input/input-06.txt")
       (str/split-lines)
       (map (fn [point-string]
              (->> (str/split point-string #", ")
                   (map parse-int)
                   (vec))))))


(defn x
  [point]
  (first point))


(defn y
  [point]
  (second point))


(defn find-extreme
  [points axis-fn end]
  (let [compare-fn (if (= end :high) > <)]
    (reduce (fn [leftmost point]
              (if (compare-fn (axis-fn point) (axis-fn leftmost))
                point
                leftmost)) points)))


(defn abs
  {:test (fn []
           (is (= 5 (abs 5)))
           (is (= 5 (abs -5))))}
  [x]
  (if (> 0 x) (- x) x))


(defn distance
  {:test (fn []
           (is (= 4 (distance [1 1] [3 3]))))}
  [[x1 y1] [x2 y2]]
  (+ (abs (- x2 x1)) (abs (- y2 y1))))


(defn find-closest-points
  [points point1]
  (reduce
    (fn [result point2]
      (let [shortest-distance (:shortest-distance result)
            distance (distance point1 point2)
            total-distance (+ distance (:total-distance result))]
        (->
          (cond (or (nil? shortest-distance) (< distance shortest-distance))
                {:shortest-distance distance
                 :closest-points    [point2]}
                (= distance shortest-distance)
                (update result :closest-points conj point2)
                (> distance shortest-distance)
                result)
          (assoc :total-distance total-distance))))
    {:shortest-distance nil :closest-points [] :total-distance 0} points))


(defn get-perimeter
  [points]
  [(x (find-extreme points x :low))
   (x (find-extreme points x :high))
   (y (find-extreme points y :low))
   (y (find-extreme points y :high))])


(defn get-all-distances
  [points]
  (let [[left right bottom top] (get-perimeter points)]
    (loop [y-axis (range bottom (inc top))
           result []]
      (if (empty? y-axis)
        result
        (let [x-axis (range left (inc right))
              y1 (first y-axis)
              new-result (->> (map (fn [x1]
                                     (-> (find-closest-points points [x1 y1])
                                         (assoc :this-point [x1 y1])))
                                   x-axis)
                              (concat result))]
          (recur (rest y-axis)
                 new-result))))))


(defn on-perimeter?
  {:test (fn []
           (let [points [[1 1] [1 5] [5 5] [5 1]]]
             (is (on-perimeter? points [5 3]))))}
  [points [x1 y1]]
  (let [[left right bottom top] (get-perimeter points)]
    (or (= x1 left) (= x1 right) (= y1 bottom) (= y1 top))))


(defn get-points-with-infinite-reach
  [points distances]
  (->>
    (filter (fn [distance-data]
              (on-perimeter? points (:this-point distance-data))) distances)
    (filter (fn [distance-data]
              (= 1 (count (:closest-points distance-data)))))
    (map (fn [distance-data]
           (first (:closest-points distance-data))))
    (distinct)))


(defn get-first-answer
  []
  (let [points (read-input)
        distances (get-all-distances points)
        infinite-reach (get-points-with-infinite-reach points distances)
        finite-reach (filter (fn [point]
                               (not (some #(= % point) infinite-reach))) points)
        closest-locations (reduce (fn [result distance-data]
                                    (let [closest-points (:closest-points distance-data)
                                          point (first closest-points)]
                                      (if (and (= 1 (count closest-points))
                                               (some #(= % point) finite-reach))
                                        (if (get result point)
                                          (update result point inc)
                                          (assoc result point 1))
                                        result))) {} distances)]
    closest-locations))


(defn get-second-answer
  []
  (let [points (read-input)
        distances (get-all-distances points)
        safe-region (filter #(> 10000 (:total-distance %)) distances)]
    (println (count distances))
    (count safe-region)))
