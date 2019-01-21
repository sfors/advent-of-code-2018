(ns advent-of-code-2018.day3
  (:require [clojure.string :as str]))


(defn get-details-from-line
  [line]
  (let [[number position size] (str/split line #" @ |: | ")]
    {:id     (subs number 1)
     :x-pos  (read-string (first (str/split position #",")))
     :y-pos  (read-string (second (str/split position #",")))
     :width  (read-string (first (str/split size #"x")))
     :height (read-string (second (str/split size #"x")))}))


(defn read-input
  []
  (->> (slurp "resources/input/input-03.txt")
       (clojure.string/split-lines)
       (map get-details-from-line)))


(defn get-claimed-positions
  [{:keys [x-pos y-pos width height]}]
  (for [x (range x-pos (+ x-pos width))
        y (range y-pos (+ y-pos height))]
    (vector x y)))


(def get-claimed-positions-memo (memoize get-claimed-positions))


(defn overlapped-positions
  [claims]
  (->> claims
       (reduce (fn [claimed-positions claim]
                 (->>
                   (get-claimed-positions-memo claim)
                   (concat claimed-positions))) '())
       (frequencies)
       (filter (fn [[_ frequency]]
                 (>= frequency 2)))
       (map first)))


(defn get-first-answer
  []
  (->> (read-input)
       (overlapped-positions)
       (count)))


(defn position-overlapped?
  [position overlaps]
  (some #(= % position) overlaps))


(def position-overlapped?-memo (memoize position-overlapped?))


(defn contains-overlap?
  [claim overlaps]
  (do (println claim)
      (let [claimed-positions (get-claimed-positions-memo claim)]
        (some #(position-overlapped?-memo % overlaps) claimed-positions))))


(defn get-second-answer
  []
  (let [input (read-input)
        overlaps (overlapped-positions input)]
    (->>
      (some (fn [claim]
              (when-not (contains-overlap? claim overlaps) claim)) input)
      :id)))
