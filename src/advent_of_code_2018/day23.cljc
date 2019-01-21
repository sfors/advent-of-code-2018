(ns advent-of-code-2018.day23
  (:require [clojure.string :as str]
            [clojure.test :refer [is]]))



(defn read-input
  [mode]
  (let [filename (if (= mode :test) "input-23-2.txt" "input-23.txt")]
    (->> (slurp (str "resources/input/" filename))
         (str/split-lines)
         (map-indexed (fn [index line]
                        (let [[_ x-str y-str z-str r-str] (re-find #"pos=<(-?\d+),(-?\d+),(-?\d+)>, r=(\d+)" line)
                              x (Integer/parseInt x-str)
                              y (Integer/parseInt y-str)
                              z (Integer/parseInt z-str)
                              r (Integer/parseInt r-str)]
                          {:id index :pos [x y z] :radius r}))))))



(defn abs
  {:test (fn []
           (is (= 5 (abs 5)))
           (is (= 5 (abs -5))))}
  [x]
  (if (> 0 x) (- x) x))


(defn distance
  {:test (fn []
           (is (= 6 (distance [1 1 1] [3 3 3]))))}
  [[x1 y1 z1] [x2 y2 z2]]
  (+ (abs (- x2 x1)) (abs (- y2 y1)) (abs (- z2 z1))))

;=> {:pos [17897573 57698360 21200645], :score 906, :distance 96796578}
; [15048182 53609301 15456525]
(defn get-points-within-radius
  [[xpos ypos zpos] radius]
  (for [x (range (- 0 radius) (+ 0 radius 1))
        y (range (- 0 (- radius (abs x))) (+ 0 (- radius (abs x)) 1))
        z (range (- 0 (- radius (+ (abs x) (abs y)))) (+ 0 (- radius (+ (abs x) (abs y))) 1))]
    [(+ xpos x) (+ ypos y) (+ zpos z)]))


(defn get-first-answer
  [mode]
  (let [nanobots (read-input mode)
        strongest (apply max-key :radius nanobots)
        within-range (filter (fn [nanobot]
                               (let [distance (distance (:pos nanobot) (:pos strongest))]
                                 (>= (:radius strongest) distance)))
                             nanobots)]
    (count within-range)))


(defn get-mid-point
  [nanobots]
  (let [num (count nanobots)

        [x y z] (reduce (fn [[sx sy sz] {[x y z] :pos}]
                          [(+ sx x) (+ sy y) (+ sz z)]) [0 0 0] nanobots)
        mid-point [(int (/ x num)) (int (/ y num)) (int (/ z num))]]
    mid-point))



(defn in-range-of
  [nanobots focus-pos]
  (count (filter (fn [{pos :pos radius :radius}]
                   (>= radius (distance pos focus-pos))) nanobots)))

(def in-range-of-memo
  (memoize in-range-of))


(defn exp [x e]
  (reduce * (repeat e x)))


(def points-close-to-zero
  (get-points-within-radius [0 0 0] 15))


(def axis (map #(exp 10 %) (range 0 12)))

(def with-neg
  (concat (map - axis) axis))

(def points-within-50 (get-points-within-radius [0 0 0] 50))

(defn get-neighbors
  [[x y z :as pos]]
  (let [x-points (map (fn [x-offset]
                        [(+ x x-offset) y z]) with-neg)
        y-points (map (fn [y-offset]
                        [x (+ y y-offset) z]) with-neg)
        z-points (map (fn [z-offset]
                        [x y (+ z z-offset)]) with-neg)
        diagonal (for [point points-close-to-zero
                       mu with-neg]
                   (map + pos (map #(* % mu) point)))
        adjacent (reduce (fn [result point]
                           (conj result (map + pos point))) [] points-within-50)]
    (distinct (concat adjacent diagonal z-points y-points x-points))))


(defn get-focus-pos
  [nanobots starting-pos]
  (let [start {:pos      starting-pos
               :score    (in-range-of-memo nanobots starting-pos)
               :distance (distance starting-pos [0 0 0])}]
    (loop [best start
           visited [start]
           tick 0]
      (let [neighbors (map (fn [pos]
                             {:pos      pos
                              :score    (in-range-of-memo nanobots pos)
                              :distance (distance pos [0 0 0])})
                           (get-neighbors (:pos best)))
            best-neighbors (:best-neighbors
                             (reduce (fn [{best-score :best-score best-neighbors :best-neighbors} neighbor]
                                       (if (empty? best-neighbors)
                                         {:best-score     (:score neighbor)
                                          :best-neighbors [neighbor]}
                                         (let [best-distance (:distance (first best-neighbors))
                                               this-distance (:distance neighbor)]
                                           (cond
                                             (> (:score neighbor) best-score)
                                             {:best-score     (:score neighbor)
                                              :best-neighbors [neighbor]}

                                             (= (:score neighbor) best-score)
                                             (cond (< this-distance best-distance)
                                                   {:best-score     (:score neighbor)
                                                    :best-neighbors [neighbor]}

                                                   (= this-distance best-distance)
                                                   {:best-score     best-score
                                                    :best-neighbors (conj best-neighbors neighbor)}

                                                   (> this-distance best-distance)
                                                   {:best-score     best-score
                                                    :best-neighbors best-neighbors}
                                                   )

                                             (< (:score neighbor) best-score)
                                             {:best-score     best-score
                                              :best-neighbors best-neighbors})))
                                       )
                                     {:best-score 0 :best-neighbors []}
                                     neighbors))
            next (first (remove (fn [neighbor]
                                  (some #(= neighbor %) visited)) best-neighbors))]
        (when (= 0 (mod tick 20)))
        (println "next" next)
        (if (some? next)
          (recur next (conj visited next) (inc tick))
          best)))))


(defn connected?
  [n1 n2]
  (let [distance (distance (:pos n1) (:pos n2))]
    (or (>= (:radius n1) distance) (>= (:radius n2) distance))))

(defn connected2?
  [n1 n2]
  (let [distance (distance (:pos n1) (:pos n2))]
    (>= (:radius n2) distance)))

(defn get-most-connected
  [nanobots]
  (reduce (fn [result n1]
            (let [connected (filter (fn [n2] (connected? n1 n2)) nanobots)
                  nanobot (assoc n1 :connected (count connected))]
              (conj result nanobot))
            ) [] nanobots))


(defn get-most-connected-from-others
  [nanobots]
  (reduce (fn [result n1]
            (let [connected (filter (fn [n2] (connected2? n1 n2)) nanobots)
                  nanobot (assoc n1 :connected (count connected))]
              (conj result nanobot))
            ) [] nanobots))


(defn get-mid-point2
  [nanobots]
  (let [nanobots (get-most-connected nanobots)
        total-connected (->> nanobots
                             (map :connected)
                             (reduce +))
        [x y z] (reduce (fn [[sx sy sz] {[x y z] :pos connected :connected}]
                          (let [connected-ratio (/ (+ 0.0 connected) total-connected)]
                            [(+ sx (* x connected-ratio)) (+ sy (* y connected-ratio)) (+ sz (* z connected-ratio))]))
                        [0 0 0]
                        nanobots)
        mid-point [(int x) (int y) (int z)]]
    mid-point))



(defn get-second-answer
  [mode]
  (let [nanobots (read-input mode)
        ;mid-point 
        ;
        ;(get-mid-point (filter (fn [nano]
        ;                         (some #(= (:id nano) %) [581 697])
        ;                         ) nanobots))
        ]
    (get-focus-pos nanobots [17897573 57698360 21200645])))


