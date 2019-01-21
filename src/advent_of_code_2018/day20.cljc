(ns advent-of-code-2018.day20
  (:require [clojure.string :as str]
            [clojure.core.reducers :refer [fold]]
            [clojure.test :refer [is]]))


(defn read-input
  []
  (slurp "resources/input/input-20.txt"))


(declare get-distances)


(defn get-branches
  [input]
  (if (not (= (first input) \())
    input
    (loop [branches []
           current-branch []
           to-parse (pop input)]
      (let [c (peek to-parse)]
        (cond
          (= c \))
          {:branches (conj branches current-branch) :remaining (pop to-parse)}

          (= c \()
          (let [{sub-branches :branches remaining :remaining} (get-branches to-parse)]
            (recur branches (conj current-branch sub-branches) remaining))

          (= c \|)
          (recur (conj branches current-branch) [] (pop to-parse))

          :else
          (recur branches (conj current-branch c) (pop to-parse)))))))


(defn get-value
  [path]
  (reduce (fn [result step]
            (if (vector? step)
              (if (empty? (last step))
                result
                (+ result (->> step
                               (map get-value)
                               (sort)
                               (last))))
              (inc result)))
          0
          path))


(def move-map
  {\W [-1 0]
   \E [1 0]
   \N [0 -1]
   \S [0 1]})


(defn update-distance
  [distances pos distance]
  (if (some? (get distances pos))
    (update distances pos min distance)
    (assoc distances pos distance)))


(def get-distances-memo
  (memoize get-distances))


(defn get-distances
  [path distances current-pos current-distance]
  ;(println "p" path)
  ;(println "d" distances)
  ;(println "pos" current-pos)
  ;(println "curdis" current-distance)
  (loop [path path
         distances distances
         current-pos current-pos
         current-distance current-distance]
    (if (empty? path)
      {:distances distances :end-pos current-pos}
      (let [step (first path)]
        (if (vector? step)
          (do
            ;(when (> (count distances) 9000)
            ;  (println "P:" (count path))
            ;  (println "D:" (count distances))
            ;  (println "S:" (count step)))
            (let [{distances :distances end-positions :end-positions}
                  (reduce (fn [{distances :distances end-positions :end-positions} branch]
                            (let [{new-distances :distances end-pos :end-pos} (get-distances-memo branch distances current-pos current-distance)]
                              {:distances new-distances :end-positions (conj end-positions end-pos)}
                              ))
                          {:distances distances :end-positions []} step)]
              (reduce (fn [{distances :distances} start-pos]
                        (get-distances-memo (rest path) distances start-pos (get distances start-pos)))
                      {:distances distances} end-positions)))
          (let [new-pos (map + current-pos (move-map step))
                new-distances (update-distance distances new-pos (inc current-distance))]
            (recur (rest path) new-distances new-pos (get new-distances new-pos))))))))





(defn get-path
  [input]
  (let [path (loop [done []
                    to-parse (apply list input)]
               (let [c (peek to-parse)]
                 (cond
                   (= c \^)
                   (recur done (pop to-parse))

                   (= c \$)
                   done

                   (= c \()
                   (let [{branches :branches remaining :remaining} (get-branches to-parse)]
                     (recur (conj done branches) remaining))

                   :else
                   (recur (conj done c) (pop to-parse)))))]
    path))


(defn get-shortest-distance
  [input]
  (let [path (get-path input)
        {distances :distances} (get-distances-memo path {} [0 0] 0)]
    (reduce-kv (fn [result k v]
                 (if (some? result)
                   (if (> v (second result))
                     [k v]
                     result)
                   [k v]))
               nil
               distances)))


(defn get-rooms-1000-plus-doors
  [input]
  (let [path (get-path input)
        {distances :distances} (get-distances-memo path {} [0 0] 0)]
    (count (reduce-kv (fn [result k v]
                        (if (>= v 1000)
                          result
                          (dissoc result k)))
                      distances
                      distances))))


(defn create-map
  [path start-pos links]
  (loop [current-pos start-pos
         end-positions []
         links []
         [step & remaining] path]))
