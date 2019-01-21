(ns advent-of-code-2018.day12
  (:require [clojure.string :as str]
            [clojure.test :refer [is]]))


(defn plant-at?
  {:test (fn []
           (is (plant-at? {:plants "..##.."} 2))
           (is (not (plant-at? {:plants "..##.."} 0)))
           )}
  [state index]
  (= \# (nth (:plants state) index)))


(defn pad-plants-begining
  [state]
  (->
    (if
      (boolean (re-find #"#" (subs (:plants state) 0 (min 5 (count (:plants state))))))
      (-> state
          (update :plants #(str "....." %))
          (update :first-plant-index - 5))
      state
      )))


(defn pad-plants-end
  [state]
  (if
    (boolean (re-find #"#" (subs (:plants state) (max 0 (- (count (:plants state)) 5)) (count (:plants state)))))
    (update state :plants #(str % "....."))
    state))


(defn pad-plants
  {:test (fn []
           (let [init-state {:plants "##." :first-plant-index 0}
                 next-gen (pad-plants init-state)]
             (is (= (:plants next-gen) ".....##......"))
             (is (= (:first-plant-index next-gen) -5))
             )
           (let [init-state {:plants ".###" :first-plant-index -2}
                 next-gen (pad-plants init-state)]
             (is (= (:plants next-gen) "......###....."))
             (is (= (:first-plant-index next-gen) -7))
             )
           (let [init-state {:plants "...." :first-plant-index 0}
                 next-gen (pad-plants init-state)]
             (is (= (:plants next-gen) "...."))
             (is (= (:first-plant-index next-gen) 0))
             )
           )}
  [state]
  (-> state
      (pad-plants-begining)
      (pad-plants-end))
  )


(defn read-input
  [mode]
  (let [filename (if (= mode :test) "input-12-2.txt" "input-12.txt")]
    (let [[initial-state-string _ & rule-strings] (-> (slurp (str "resources/input/" filename))
                                                      (str/split-lines))
          rules (reduce (fn [rule-map rule-string]
                          (let [[in out] (str/split rule-string #" => ")]
                            (assoc rule-map (str/trim in) out))
                          ) {} rule-strings)
          plants (nth (str/split initial-state-string #" ") 2)
          ]
      {:plants            plants
       :first-plant-index 0
       :rules             rules}
      )))

(defn get-new-plant
  [view rules]
  (let [new-plant (get rules view ".")]
    (println "new plant" new-plant)
    new-plant
    )
  )


(defn get-next-gen-plants
  [in-plants rules]
  (loop [[l2 l1 c r1 r2 :as plants] in-plants
         result ".."]
    ; (println "view" l2 l1 c r1 r2)
    (let [new-plant (get rules (str l2 l1 c r1 r2) ".")
          new-result (str result new-plant)]
      ;    (println "new plant" new-plant)
      ;(println "new result" new-result)
      ;(println "rest" (rest plants))
      (if (> 5 (count (rest plants)))
        (str new-result r1 r2)
        (recur (rest plants) new-result)))))


(defn get-next-gen
  [state]
  (-> state
      (pad-plants)
      (update :plants get-next-gen-plants (:rules state))))


(defn get-score
  {:test (fn []
           (let [state {:first-plant-index -3
                        :plants            ".#....##....#####...#######....#.#..##."}]
             (is (= 325 (:result (get-score state)))))
           (let [state {:first-plant-index -10
                        :plants            "........#....##....#####...#######....#.#..##....."}]
             (is (= 325 (:result (get-score state)))))
           )}
  [{first-plant-index :first-plant-index
    plants            :plants}]
  (reduce (fn [{result :result index :index} plant]
            (if (= \# plant)
              {:result (+ result index)
               :index  (inc index)}
              {:result result :index (inc index)}))
          {:result 0 :index first-plant-index} plants))


(defn get-first-answer
  [mode]
  (print (read-input mode))
  (loop [state (read-input mode)
         gen 0]
    (if (>= gen 50000000000)
      (get-score state)
      (do
        (println gen ":" (:first-plant-index state) ":" (:result (get-score state)) ":" (:plants state))
        (recur (get-next-gen state) (inc gen))))))


(defn get-second-answer
  []
  (let [times-left (- 50000000000 1742)
        score-left (* times-left 22)
        total (+ 38835 score-left)]
    total
    ))