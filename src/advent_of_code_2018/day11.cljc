(ns advent-of-code-2018.day11
  (:require [clojure.test :refer [is]]))


(defn get-hundreds-digit
  {:test (fn []
           (is (= 3 (get-hundreds-digit 12345)))
           (is (= 5 (get-hundreds-digit 567)))
           (is (= 0 (get-hundreds-digit 12)))
           )}
  [number]
  (let [s (str number)
        length (count s)
        index (- length 3)]
    (if (>= index 0)
      (-> (nth s index)
          (str)
          (Integer/parseInt))
      0)))


(defn get-power-level
  {:test (fn []
           (is (= 4 (get-power-level [3 5] 8)))
           (is (= 0 (get-power-level [217 196] 39)))
           )}
  [[x y] serial-number]
  (let [rack-id (+ x 10)]
    (-> rack-id
        (* y)
        (+ serial-number)
        (* rack-id)
        (get-hundreds-digit)
        (- 5))))


(defn get-power-levels
  [serial-number]
  (let [ys (range 1 301)]
    (->> (map
           (fn [y]
             (let [xs (range 1 301)]
               (reduce
                 (fn [result x]
                   (let [coordinate [x y]]
                     (assoc result coordinate (get-power-level [x y] serial-number)))
                   ) {} xs))) ys)
         (reduce merge))))


(defn get-square-power2
  {:test (fn []
           ;    (is (= 29 (get-square-power2 [33 45 3] (get-power-levels 18))))
           ;(is (= 29 (get-square-power2 [33 45 3] (get-power-levels 18))))
           ;(is (= 30 (get-square-power2 [21 61 3] (get-power-levels 42))))
           )}
  [[xpos ypos size] power-levels]
  (if (>= 1 size)
    (get-power-level [xpos ypos] power-levels))
  (let [smaller (get-square-power2 [xpos ypos (dec size)] power-levels)
        xs (range xpos (+ xpos size))
        ys (range ypos (dec (+ ypos size)))
        last-row (reduce
                   (fn [result x]
                     (let [power-level (get power-levels [x (dec (+ ypos size))])]
                       (+ result power-level))) 0 xs)
        last-column (reduce
                      (fn [result y]
                        (let [power-level (get power-levels [(dec (+ xpos size)) y])]
                          (+ result power-level))) 0 ys)]
    (+ smaller last-row last-column)))


(defn get-square-power
  {:test (fn []
           (is (= 29 (get-square-power [33 45 3] (get-power-levels 18) {})))
           (is (= 29 (get-square-power [33 45 3] (get-power-levels 18) {[33 45 2] 14})))
           (is (= 30 (get-square-power [21 61 3] (get-power-levels 42) {})))
           )}
  [[xpos ypos size] power-levels existing-squares]
  (let [existing-square (get existing-squares [xpos ypos (dec size)])
        existing-square-size (if (not (nil? existing-square)) existing-square 0)
        ymax (+ ypos (dec size))
        ys (range ypos (+ ypos size))]
    (println existing-square-size)
    (reduce
      (fn [result y]
        (println existing-square)
        (let [xs (if (and (not (nil? existing-square))
                          (= y ymax))
                   (range (dec (+ xpos size)) (+ xpos size))
                   (range xpos (+ xpos size)))
              row-result (reduce
                           (fn [result x]
                             (let [power-level (get power-levels [x y])]
                               (+ result power-level))) 0 xs)]
          (+ result row-result))) existing-square-size ys)))


(defn get-first-answer
  [serial-number]
  (let [power-levels (get-power-levels serial-number)
        sizes (range 1 300)
        power-squares
        (reduce (fn [result size]
                  (println size)
                  (let [ys (range 1 (- 301 size))]
                    (reduce
                      (fn [result y]
                        (let [xs (range 1 (- 301 size))]
                          (reduce
                            (fn [result x]
                              (let [coordinate [x y size]
                                    power-level (get-square-power coordinate power-levels result)]
                                (assoc result coordinate power-level))) result xs))) result ys)
                    )) {} sizes)]
    (apply max-key val power-squares)))
