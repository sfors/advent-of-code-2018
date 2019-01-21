(ns advent-of-code-2018.day15
  (:require [clojure.string :as str]
            [clojure.test :refer [is]]))


(defn read-input
  [mode]
  (let [filename (cond (= mode :test2)
                       "input-15-2.txt"
                       (= mode :test3)
                       "input-15-3.txt"
                       (= mode :test4)
                       "input-15-4.txt"
                       (= mode :real)
                       "input-15.txt")]
    (->> (slurp (str "resources/input/" filename))
         (str/split-lines))))

(defn get-new-unit
  [type pos]
  (let [ap (if (= type :elf) 34 3)]
    {:id (str (first pos) (second pos)) :type type :pos pos :ap ap :hp 200}))


(defn get-unit-type
  [c]
  (cond
    (= c \G) :goblin
    (= c \E) :elf
    :else :other))


(defn get-unit
  {:test (fn []
           (let [state {:units [{:id "1" :type :elf :pos [5 7]}
                                {:id "2" :type :goblin :pos [9 12]}]}]
             (is (= :elf (:type (get-unit state "1"))))
             (is (= :goblin (:type (get-unit state "2"))))
             (is (nil? (get-unit state "3")))))}
  [state id]
  (first
    (filter #(= (:id %) id) (:units state))))


(defn get-cave-row
  {:test (fn []
           (let [cave-row (get-cave-row "##.GE.##.#E..G.#" 4)]
             (is (= (:units cave-row) [{:id "34" :type :goblin :pos [3 4] :ap 3 :hp 200}
                                       {:id "44" :type :elf :pos [4 4] :ap 3 :hp 200}
                                       {:id "104" :type :elf :pos [10 4] :ap 3 :hp 200}
                                       {:id "134" :type :goblin :pos [13 4] :ap 3 :hp 200}]))
             (is (= (:cave cave-row) "##....##.#.....#"))
             )
           )}
  [input-row row-number]
  (->
    (reduce (fn [cave-row location]
              (->
                (if
                  (or (= location \#) (= location \.))
                  (update cave-row :cave conj location)
                  (let [unit (get-new-unit (get-unit-type location) [(:index cave-row) row-number])]
                    (-> cave-row
                        (update :units conj unit)
                        (update :cave conj \.))))
                (update :index inc))) {:units [] :cave [] :index 0} input-row)
    (update :cave #(apply str %))))


(defn get-state
  {:test (fn []
           (let [input ["##...E...G.##...#"
                        "#..G...####.....#"]
                 cave (get-state input)]
             (is (= (:units cave) [{:id "50" :type :elf :pos [5 0] :ap 3 :hp 200}
                                   {:id "90" :type :goblin :pos [9 0] :ap 3 :hp 200}
                                   {:id "31" :type :goblin :pos [3 1] :ap 3 :hp 200}]))
             (is (= (:cave cave) ["##.........##...#"
                                  "#......####.....#"]))))}
  [input]
  (reduce (fn [{units :units cave :cave row-number :row-number} row]
            (let [{new-units :units cave-row :cave} (get-cave-row row row-number)]
              {:units      (concat units new-units)
               :cave       (conj cave cave-row)
               :row-number (inc row-number)})
            ) {:units [] :cave [] :row-number 0} input))


(defn get-unit-in-pos
  {:test (fn []
           (let [units [{:type :elf :pos [5 7]}
                        {:type :goblin :pos [9 12]}]]
             (is (= :elf (:type (get-unit-in-pos units [5 7]))))
             (is (= :goblin (:type (get-unit-in-pos units [9 12]))))
             (is (nil? (get-unit-in-pos units [2 4])))
             )
           )}
  [units pos]
  (first
    (filter #(= (:pos %) pos) units)))


(defn draw-cave
  {:test (fn []
           (let [cave {:units [{:type :elf :pos [1 3]}
                               {:type :goblin :pos [4 0]}
                               {:type :goblin :pos [2 2]}
                               {:type :elf :pos [3 1]}]
                       :cave  ["#......##"
                               "##...#.##"
                               "##...#..#"
                               "#......##"
                               ]}
                 cave-with-units (draw-cave cave)]
             (is (= cave-with-units ["#...G..##"
                                     "##.E.#.##"
                                     "##G..#..#"
                                     "#E.....##"]))))}
  [{units :units cave :cave}]
  (map-indexed
    (fn [row-number row]
      (->>
        (map-indexed
          (fn [index location]
            (let [unit (get-unit-in-pos units [index row-number])]
              (if (some? unit)
                (if (= (:type unit) :goblin)
                  \G
                  \E)
                location)))
          row)
        (apply str)))
    cave))


(defn print-cave
  [cave]
  (doseq [row cave]
    (println row)))


(defn get-units-by-type
  [units type]
  (filter #(= (:type %) type) units))


(defn get-goblins
  [units]
  (get-units-by-type units :goblin))


(defn get-elves
  [units]
  (get-units-by-type units :elf))


(defn get-enemies
  [state id]
  (if (= (:type (get-unit state id)) :goblin)
    (get-elves (:units state))
    (get-goblins (:units state))))


(defn get-up
  {:test (fn []
           (is (= [3 4] (get-up [3 5]))))}
  [pos]
  [(first pos) (dec (second pos))])


(defn get-down
  {:test (fn []
           (is (= [3 6] (get-down [3 5]))))}
  [pos]
  [(first pos) (inc (second pos))])


(defn get-left
  {:test (fn []
           (is (= [2 5] (get-left [3 5]))))}
  [pos]
  [(dec (first pos)) (second pos)])


(defn get-right
  {:test (fn []
           (is (= [4 5] (get-right [3 5]))))}
  [pos]
  [(inc (first pos)) (second pos)])


(defn get-width
  [cave]
  (count (first cave)))


(defn get-height
  [cave]
  (count cave))


(defn reading-order-pos
  [pos {cave :cave}]
  (+ (first pos) (* (get-width cave) (second pos))))


(defn get-test-state
  []
  {:units [{:id "1" :type :elf :pos [2 2] :hp 200}
           {:id "2" :type :goblin :pos [5 6] :hp 200}
           {:id "3" :type :goblin :pos [2 3] :hp 200}
           {:id "4" :type :goblin :pos [5 5] :hp 200}
           {:id "5" :type :goblin :pos [2 1] :hp 200}
           {:id "6" :type :elf :pos [6 5] :hp 60}
           {:id "7" :type :efl :pos [5 4] :hp 150}
           {:id "8" :type :elf :pos [4 5] :hp 30}]
   :cave  ["###########"
           "#.........#"
           "#.........#"
           "#.........#"
           "#.........#"
           "#.........#"
           "###########"]})


(defn get-position
  [state id]
  (:pos (get-unit state id)))


(defn get-adjacent-positions
  [position]
  [{:direction :up :pos (get-up position)}
   {:direction :left :pos (get-left position)}
   {:direction :right :pos (get-right position)}
   {:direction :down :pos (get-down position)}])


(defn get-adjacent-enemy
  {:test (fn []
           (let [id "1"
                 state (get-test-state)
                 first-adjacent (get-adjacent-enemy state id)]
             (is (= "5" first-adjacent)))
           (let [id "4"
                 state (get-test-state)
                 first-adjacent (get-adjacent-enemy state id)]
             (is (= "8" first-adjacent)))
           (let [id "2"
                 state (get-test-state)
                 first-adjacent (get-adjacent-enemy state id)]
             (is (nil? first-adjacent))))}
  [state id]
  (let [position (get-position state id)
        adjacent-positions (get-adjacent-positions position)
        adjacent-enemies (->> adjacent-positions
                              (map :pos)
                              (map #(get-unit-in-pos (get-enemies state id) %))
                              (remove nil?)
                              (sort-by (juxt :hp #(reading-order-pos (:pos %) state))))]
    (:id (first adjacent-enemies))))


(defn take-dmg
  [unit dmg]
  (update unit :hp - dmg))


(defn attack
  {:test (fn []
           (let [unit1 {:id "1" :type :goblin :pos [5 5] :ap 3 :hp 200}
                 unit2 {:id "2" :type :elf :pos [5 6] :ap 3 :hp 200}
                 unit3 {:id "3" :type :elf :pos [3 3] :ap 3 :hp 2}
                 pre-state {:units [unit1 unit2 unit3]}
                 state (-> pre-state
                           (attack "2" "3")
                           (attack "1" "2"))]
             (is (= 197 (:hp (get-unit state "2"))))
             (is (nil? (get-unit state "3")))
             ))}
  [state attacker defendant]
  (let [dmg (:ap (get-unit state attacker))]
    (assoc state :units (reduce (fn [units unit]
                                  (if (= defendant (:id unit))
                                    (let [damaged-unit (take-dmg unit dmg)]
                                      (if (> (:hp damaged-unit) 0)
                                        (conj units damaged-unit)
                                        units))
                                    (conj units unit))) [] (:units state)))))


(defn blocked?
  [{units :units} position]
  (some #(= position %) (map :pos units)))


(defn walkable?
  {:test (fn []
           (let [state {:cave ["...#."
                               "##..#"]}]
             (is (walkable? state [2 0]))
             (is (not (walkable? state [0 1])))))}
  [{cave :cave} [x y]]
  (= \. (nth (nth cave y) x)))


(defn in?
  [coll element]
  (some #(= element %) coll))


(defn get-adjacent-available-positions
  [state pos]
  (reduce (fn [positions {pos :pos}]
            (if (and (walkable? state pos)
                     (not (blocked? state pos)))
              (conj positions pos)
              positions)) [] (get-adjacent-positions pos)))


(defn find-fighting-positions
  {:test (fn []
           (let [state (get-state ["#######"
                                   "#.E...#"
                                   "#.....#"
                                   "#...GG#"
                                   "#.....#"
                                   "#######"])
                 for-elf (find-fighting-positions state "21")
                 for-goblins (find-fighting-positions state "43")]
             (is (= for-elf [[4 2] [5 2] [3 3] [4 4] [5 4]]))
             (is (= for-goblins [[1 1] [3 1] [2 2]]))
             ))}
  [state id]
  (->>
    (reduce (fn [fighting-positions enemy]
              (concat fighting-positions
                      (get-adjacent-available-positions state (:pos enemy))))
            [] (get-enemies state id))
    (sort-by #(reading-order-pos % state))))


(defn update-visited
  {:test (fn []
           (let [visited {[3 3] {:distance 0}
                          [5 4] {:distance 4 :from [4 2]}
                          [5 8] {:distance 9 :from [2 4]}}
                 new-visited (-> visited
                                 (update-visited {:to [5 4] :distance 3 :from [9 9]})
                                 (update-visited {:to [5 8] :distance 11 :from [7 7]})
                                 (update-visited {:to [4 4] :distance 2 :from [14 3]}))]
             (is (= (get new-visited [5 4]) {:distance 3 :from [9 9]}))
             (is (= (get new-visited [5 8]) {:distance 9 :from [2 4]}))
             (is (= (get new-visited [3 3]) {:distance 0}))
             (is (= (get new-visited [4 4]) {:distance 2 :from [14 3]}))))}
  [visited {to :to from :from distance :distance}]
  (let [move {:from from :distance distance}]
    (if (some? (get visited to))
      (update visited to
              (fn [old new]
                (if (> (:distance old) (:distance new))
                  new
                  old)) move)
      (assoc visited to move))))


(defn get-reachable-positions
  [state id]
  (let [start-pos (get-position state id)]
    (loop [visited {start-pos {:distance 0}}
           to-explore []
           {distance :distance current-pos :pos} {:pos start-pos :distance 0}]
      ;(println "visited" visited)
      ;(println "to-exp" to-explore)
      ;(println "distance" distance "current-pos" current-pos)
      (let [available-moves (get-adjacent-available-positions state current-pos)
            ;moves-in-fighting-pos (filter #(in? fighting-positions (:pos %)) available-moves)
            new-moves (reduce (fn [result move]
                                (if (not (some? (get visited move)))
                                  (conj result {:from     current-pos
                                                :to       move
                                                :distance (inc distance)})
                                  result)) [] available-moves)
            new-visited (reduce (fn [result move]
                                  (assoc result (:to move) (dissoc move :to))) visited new-moves)
            new-to-explore (reduce (fn [result {to :to distance :distance}]
                                     (conj result {:pos to :distance distance})) to-explore new-moves)]
        ;(println "current" current-pos "available moves" available-moves "new-moves" new-moves)
        ;(println "explore" new-to-explore)
        ;(println "new-visited" new-visited)
        (if (empty? new-to-explore)
          new-visited
          (recur new-visited (vec (rest new-to-explore)) (first new-to-explore)))))))


(defn get-reachable-fighting-positions
  [state id reachable-positions]
  (let [fighting-positions (find-fighting-positions state id)]
    ;(println "figthing positions" fighting-positions)
    (reduce (fn [result pos]
              (let [reachable-fighting-position (get reachable-positions pos)]
                ;(println "reach fight pos" reachable-fighting-position)
                (if (some? reachable-fighting-position)
                  (conj result {:pos      pos
                                :distance (:distance reachable-fighting-position)})
                  result))
              ) [] fighting-positions)))


(defn get-closest-fighting-position
  [reachable-fighting-positions]
  (reduce (fn [result pos]
            (let [current-closest-distance (:distance (first result))]
              (cond (> current-closest-distance (:distance pos))
                    [pos]
                    (= current-closest-distance (:distance pos))
                    (conj result pos)
                    (< current-closest-distance (:distance pos))
                    result)))
          [(first reachable-fighting-positions)]
          (rest reachable-fighting-positions)))


(defn get-next-pos
  [state id]
  (let [reachable-positions (get-reachable-positions state id)
        reachable-fighting-positions (get-reachable-fighting-positions state id reachable-positions)
        closest-fighting-positions (get-closest-fighting-position reachable-fighting-positions)
        closest-fighting-pos (first (sort-by #(reading-order-pos (:pos %) state) closest-fighting-positions))
        path (loop [current-pos (:pos closest-fighting-pos)
                    path (list (:pos closest-fighting-pos))]
               (let [next (:from (get reachable-positions current-pos))]
                 (if (some? next)
                   (recur next (cons next path))
                   path)))]
    ;(println "start-pos" (get-position state id))
    ;(println "reach" reachable-positions)
    ;(println "reach fight" reachable-fighting-positions)
    ;(println "closest" closest-fighting-pos)
    ;(println "path" path)
    (second path)))


(defn move
  [state id]
  (update state :units (fn [units]
                         (reduce (fn [result unit]
                                   (conj result (if (= (:id unit) id)
                                                  (let [next-pos (get-next-pos state id)]
                                                    (if (some? next-pos)
                                                      (assoc unit :pos next-pos)
                                                      unit))
                                                  unit))) [] units)))
  )


(defn do-turn
  [state id]
  (let [first-adjacent (get-adjacent-enemy state id)]
    (if
      (some? first-adjacent)
      (attack state id first-adjacent)
      (as-> state $
            (move $ id)
            (let [first-adjacent (get-adjacent-enemy $ id)]
              (if
                (some? first-adjacent)
                (attack $ id first-adjacent)
                $))))))


(defn sort-units
  [state]
  (update state :units (fn [units]
                         (sort-by #(reading-order-pos (:pos %) state) units))))


(defn number-of-elves
  [state]
  (count (get-elves (:units state))))


(defn do-one-turn
  [in-state]
  (loop [state (sort-units in-state)
         done []
         todo (map :id (:units state))]
    (let [id (first todo)
          alive (not (nil? (get-unit state id)))
          new-state (if alive (do-turn state id) state)
          new-done (if alive (conj done id) done)]
      (if (empty? (rest todo))
        new-state
        (recur new-state new-done (vec (rest todo)))))))


(defn sum-hp
  [state]
  (->> (map :hp (:units state))
       (reduce +)))


(defn get-first-answer
  [in-state]
  (let [elves (number-of-elves in-state)]
    (loop [state in-state
           round 0]
      (let [units (:units state)
            any-enemies (seq (get-enemies state (:id (first units))))]
        (if any-enemies
          (let [new-state (do-one-turn state)]
            (print-cave (draw-cave new-state))
            (recur new-state (inc round)))
          (let [hp-sum (sum-hp state)
                score (* hp-sum (dec round))
                elf-deaths (- elves (number-of-elves state))]
            {:state state
             :round (dec round)
             :hp-sum hp-sum
             :score score
             :elf-deaths elf-deaths}))))))
