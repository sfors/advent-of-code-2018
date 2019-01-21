(ns advent-of-code-2018.day18
  (:require [clojure.string :as str]
            [clojure.test :refer [is]]))

(defn read-input
  []
  (->> (slurp (str "resources/input/input-18.txt"))
       (str/split-lines)))


(defn max-x
  [area]
  (count (first area)))


(defn max-y
  [area]
  (count area))


(defn get-element
  [area [x y]]
  (nth (nth area y []) x \x))


(defn do-magic
  [area [x y]]
  (let [this (get-element area [x y])
        up (get-element area [x (dec y)])
        down (get-element area [x (inc y)])
        left (get-element area [(dec x) y])
        right (get-element area [(inc x) y])
        lu (get-element area [(dec x) (dec y)])
        ru (get-element area [(inc x) (dec y)])
        ld (get-element area [(dec x) (inc y)])
        rd (get-element area [(inc x) (inc y)])
        surrounding (-> {\. 0 \| 0 \# 0 \x 0}
                        (update up inc)
                        (update down inc)
                        (update left inc)
                        (update right inc)
                        (update lu inc)
                        (update ru inc)
                        (update ld inc)
                        (update rd inc))]
    (cond
      (= this \.)
      (if (>= (get surrounding \|) 3)
        \|
        \.)
      (= this \|)
      (if (>= (get surrounding \#) 3)
        \#
        \|)
      (= this \#)
      (if (and (>= (get surrounding \#) 1) (>= (get surrounding \|) 1))
        \#
        \.))))

(defn get-first-answer
  [area]
  (let [finished
        (loop [area area
               tick 0]
          (if (= tick 1000000000)
            area
            (let [new-area
                  (map (fn [y]
                         (apply str
                                (map
                                  (fn [x]
                                    (do-magic area [x y]))
                                  (range 0 (max-x area)))))
                       (range 0 (max-y area)))]
              (when (and (>= tick 3600) (<= tick 4300))
                (println tick (reduce (fn [result element]
                                        (update result element inc))
                                      {\| 0 \# 0 \. 0}
                                      (reduce concat new-area))))
              (recur new-area (inc tick)))))
        sums (reduce (fn [result element]
                       (update result element inc))
                     {\| 0 \# 0 \. 0}
                     (reduce concat finished))]
    (* (get sums \|) (get sums \#))))

; period (t 3600, \| 608, \# 311 \. 1581) (3628 \| 608, \# 311, \. 1581)


;3601 {| 608, # 311, . 1581}
;3602 {| 610, # 314, . 1576}
;3603 {| 603, # 331, . 1566}
;3604 {| 596, # 334, . 1570}
;3605 {| 581, # 343, . 1576}
;3606 {| 574, # 325, . 1601}
;3606 {| 566, # 322, . 1612}
;3608 {| 562, # 314, . 1624}
;3609 {| 556, # 313, . 1631}
;3610 {| 552, # 308, . 1640}
;3611 {| 549, # 305, . 1646}
;3612 {| 554, # 291, . 1655}
;3613 {| 562, # 293, . 1645}
;3614 {| 577, # 287, . 1636}
;3615 {| 593, # 290, . 1617}
;3616 {| 610, # 290, . 1600}
;3617 {| 627, # 292, . 1581}
;3618 {| 645, # 294, . 1561}
;3619 {| 662, # 299, . 1539}
;3620 {| 679, # 303, . 1518}
;3621 {| 693, # 312, . 1495}
;3622 {| 681, # 317, . 1502}
;3623 {| 668, # 322, . 1510}
;3624 {| 660, # 326, . 1514}
;3625 {| 648, # 336, . 1516}
;3627 {| 644, # 338, . 1518}
;3627 {| 626, # 351, . 1523}
;3628 {| 607, # 354, . 1539}
;3629 {| 608, # 311, . 1581}

(3629 - 3600) % 28 = 29 % 28 = 1

(comment (read-input))
