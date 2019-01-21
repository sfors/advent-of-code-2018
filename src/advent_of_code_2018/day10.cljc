(ns advent-of-code-2018.day10
  (:require [clojure.string :as str]
            [clojure.math.numeric-tower :as math]
            [clojure.test :refer [is]]))


(defn read-input
  [mode]
  (let [filename (if (= mode :test) "input-10-2.txt" "input-10.txt")]
    (->> (slurp (str "resources/input/" filename))
         (str/split-lines)
         (map (fn [s] (->> (re-seq #"-?\d+" s)
                           (map #(Integer/parseInt %)))))
         (map (fn [[xpos ypos xvel yvel]]
                {:xpos xpos :ypos ypos
                 :xvel xvel :yvel yvel})))))


(defn distance [[x1 y1] [x2 y2]]
  (let [distance-x (- x1 x2)
        distance-y (- y1 y2)]
    (math/sqrt (+ (* distance-x distance-x) (* distance-y distance-y)))))


(defn get-middle-point
  [lights]
  (let [nlights (count lights)]
    (->
      (reduce (fn [{result-xpos :xpos result-ypos :ypos}
                   {xpos :xpos ypos :ypos}]
                {:xpos (+ result-xpos xpos)
                 :ypos (+ result-ypos ypos)}) lights)
      (update :xpos #(/ % nlights))
      (update :ypos #(/ % nlights)))))


(defn spread
  [lights]
  (let [{mid-xpos :xpos mid-ypos :ypos} (get-middle-point lights)]
    (->>
      (map (fn [{xpos :xpos ypos :ypos}]
             (distance [xpos ypos] [mid-xpos mid-ypos])) lights)
      (reduce +))))


(defn update-positions
  [lights]
  (map (fn [{xpos :xpos ypos :ypos
             xvel :xvel yvel :yvel}]
         {:xpos (+ xpos xvel)
          :ypos (+ ypos yvel)
          :xvel xvel :yvel yvel}) lights))


(defn abs
  {:test (fn []
           (is (= 5 (abs 5)))
           (is (= 5 (abs -5))))}
  [x]
  (if (> 0 x) (- x) x))

(defn draw
  [lights]
  (let [max-xpos (:xpos (apply max-key :xpos lights))
        min-xpos (:xpos (apply min-key :xpos lights))
        max-ypos (:ypos (apply max-key :ypos lights))
        min-ypos (:ypos (apply min-key :ypos lights))
        positions (map (fn [{xpos :xpos ypos :ypos}]
                         [xpos ypos]) lights)]
    (if (> 200 (abs (- max-xpos min-xpos)) (abs (- min-xpos min-ypos)))
      (doall
        (->>
          (map (fn [y]
                 (map (fn [x]
                        (->>
                          (if (some #(= [x y] %) positions)
                            "#"
                            ".")
                          (apply str)))
                      (range min-xpos (inc max-xpos))))
               (range min-ypos (inc max-ypos)))
          (map println))))))


(defn calc
  [lights]
  (loop [x 0
         lights lights]
    ;; (when (= 0 (mod x 100000))
    ;;  (println "x:" x "Spread: " (spread lights))
    (let [new-lights (update-positions lights)
          old-spread (spread lights)
          new-spread (spread new-lights)]
      (if (> new-spread old-spread)
        (do
          (println x)
          (draw lights))
        (recur (inc x) new-lights)))))

