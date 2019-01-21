(ns advent-of-code-2018.day24
  (:require [clojure.string :as str]
            [clojure.test :refer [is]]))


;Immune System:
;17 units each with 5390 hit points (weak to radiation bludgeoning) with
;an attack that does 4507 fire damage at initiative 2
;989 units each with 1274 hit points (immune to fire; weak to bludgeoning
;slashing) with an attack that does 25 slashing damage at initiative 3
;
;Infection:
;801 units each with 4706 hit points (weak to radiation) with an attack
;that does 116 bludgeoning damage at initiative 1
;4485 units each with 2961 hit points (immune to radiation; weak to fire
;cold) with an attack that does 12 slashing damage at initiative 4

(defn get-test-state
  []
  {:immune-system [{:id     1
                    :units  17
                    :hp     5390
                    :attack {:damage 4507 :type :fire}
                    :im     2
                    :weak   #{:bludgeoning :radiation}}
                   {:id     2
                    :units  989
                    :hp     1274
                    :attack {:damage 25 :type :slashing}
                    :im     3
                    :immune #{:fire}
                    :weak   #{:bludgeoning :slashing}}]
   :infection     [{:id     3
                    :units  801
                    :hp     4706
                    :attack {:damage 116
                             :type   :bludgeoning}
                    :im     1
                    :weak   #{:radiation}}
                   {:id     4
                    :units  4485
                    :hp     2961
                    :attack {:damage 12 :type :slashing}
                    :im     4
                    :immune #{:radiation}
                    :weak   #{:fire :cold}}]})


(defn property-from-string
  {:test (fn []
           (is (= (property-from-string "weak to bludgeoning, radiation")
                  {:weak #{:bludgeoning :radiation}}))
           (is (= (property-from-string "immune to fire")
                  {:immune #{:fire}}))
           )}
  [s]
  (when (some? s)
    (let [[_ prop-string types-string] (re-find #"(immune|weak) to ([ \w,]+)" s)
          prop (keyword prop-string)
          types (set (map keyword (str/split types-string #", ")))]
      {prop types})))


(defn get-group
  [{immune-system :immune-system infection :infection} id]
  (first (filter #(= id (:id %)) (concat immune-system infection))))


(defn get-im
  [state id]
  (:im (get-group state id)))


(defn get-hp
  [state id]
  (:hp (get-group state id)))


(defn get-units
  [state id]
  (:units (get-group state id)))


(defn get-all-groups
  [state]
  (map :id (concat (:immune-system state) (:infection state))))


(defn alive?
  [state id]
  (let [group (get-group state id)]
    (and (some? group)
         (> (get-units state id) 0))))


(defn get-army-from-group?
  [state id]
  (cond
    (some #(= id (:id %)) (:immune-system state))
    :immune-system

    (some #(= id (:id %)) (:infection state))
    :infection

    :else
    :none))
;39 9502
;38 10191
;37 9495
;36 9174

(defn read-input
  [mode]
  (let [filename (if (= mode :test) "input-24-2.txt" "input-24.txt")]
    (as-> (slurp (str "resources/input/" filename)) $
          (str/split-lines $)
          (reduce (fn [result line]
                    (if (empty? line)
                      result
                      (cond
                        (= line "Immune System:")
                        (assoc result :army :immune-system)

                        (= line "Infection:")
                        (assoc result :army :infection)

                        :else
                        (let [[_ units hp prop-string1 prop-string2 damage type im]
                              (re-find #"(\d+) units each with (\d+) hit points (?:\(([ \w,]+)?(?:; )?([ \w,]+)?\) )?with an attack that does (\d+) (\w+) damage at initiative (\d+)" line)
                              prop1 (property-from-string prop-string1)
                              prop2 (property-from-string prop-string2)
                              group (-> {:id     (:next-id result)
                                         :units  (Integer/parseInt units)
                                         :hp     (Integer/parseInt hp)
                                         :attack {:damage (if (= (:army result) :immune-system)
                                                            (+ 35 (Integer/parseInt damage))
                                                            (Integer/parseInt damage))
                                                  :type   (keyword type)}
                                         :im     (Integer/parseInt im)}
                                        (into prop1)
                                        (into prop2))]
                          (-> result
                              (update (:army result) conj group)
                              (update :next-id inc))))))
                  {:army          :none
                   :next-id       1
                   :immune-system []
                   :infection     []}
                  $)
          (dissoc $ :army :next-id))))

;18 units each with 729 hit points (weak to fire; immune to cold slashing)
;with an attack that does 8 radiation damage at initiative 10
(defn get-effective-power
  {:test (fn []
           (is (= 144 (get-effective-power {:immune-system []
                                            :infection     [{:id     3
                                                             :units  18
                                                             :hp     729
                                                             :attack {:damage 8
                                                                      :type   :radiation}}]} 3))))}
  [state id]
  (let [{units :units {damage :damage} :attack} (get-group state id)]
    (* damage units)))


(defn immune?
  [state attacker defender]
  (let [{{type :type} :attack} (get-group state attacker)
        {immune :immune} (get-group state defender)]
    (contains? immune type)))


(defn super-effective?
  [state attacker defender]
  (let [{{type :type} :attack} (get-group state attacker)
        {weak :weak} (get-group state defender)]
    (contains? weak type)))


(defn get-damage
  [state attacker defender]
  (let [damage (cond
                 (immune? state attacker defender) 0
                 (super-effective? state attacker defender) (* 2 (get-effective-power state attacker))
                 :else (get-effective-power state attacker))
        defender-group (get-group state defender)
        units (:units defender-group)
        hp (:hp defender-group)
        potentially-killed (int (/ damage hp))
        units-left (max 0 (- units potentially-killed))
        units-killed (- units units-left)
        actual-damage (* units-killed hp)]
    ;
    ;(if (> actual-damage 0)
    ;  damage
    ;  (do
    ;    ; (println "units" units)
    ;    ;(println "hp" hp)
    ;    ;(println "potentially-killed" potentially-killed)
    ;    ;(println "units left" units-left)
    ;    ;(println "units killed" units-killed)
    ;    ;(println actual-damage)
    ;    0)
    ;  
    ;  )
    damage
    ))


(defn get-potential-damage
  [state attacker defender]
  (let [damage (get-damage state attacker defender)
        defender-group (get-group state defender)
        units (:units defender-group)
        hp (:hp defender-group)
        potentially-killed (int (/ damage hp))
        units-left (max 0 (- units potentially-killed))
        units-killed (- units units-left)
        actual-damage (* units-killed hp)]
    actual-damage))


(defn select-target
  {:test (fn []
           (let [[target-id targets-left] (select-target (get-test-state) 1 #{3 4})]
             (is (= target-id 4))
             (is (= targets-left #{3}))))}
  [state attacker targets]
  (let [target-id (->> targets
                       (sort-by (juxt #(get-damage state attacker %)
                                      #(get-effective-power state %)
                                      #(get-im state %)))
                       (reverse)
                       (first))
        damage (get-damage state attacker target-id)]
    (if (> damage 0)
      [target-id (disj targets target-id)]
      [nil targets])))


(defn get-groups-in-targeting-order
  {:test (fn []
           (let [[group1 group2] (get-groups-in-targeting-order (get-test-state) :immune-system)]
             (is (= group1 1))
             (is (= group2 2)))
           (let [[group1 group2] (get-groups-in-targeting-order (get-test-state) :infection)]
             (is (= group1 3))
             (is (= group2 4)))
           )}
  [state army]
  (->> (army state)
       (map :id)
       (sort-by (juxt #(get-effective-power state %)
                      #(get-im state %)))
       (reverse)))


(defn get-army
  [state army]
  (map :id (army state)))


(defn other-army
  [state army]
  (set (get-army state ({:infection     :immune-system
                         :immune-system :infection} army))))


(defn get-targets
  {:test (fn []
           (let [targets (get-targets (get-test-state) :immune-system)]
             (is (= {1 4
                     2 3} targets)))
           (let [targets (get-targets (get-test-state) :infection)]
             (is (= {3 1
                     4 2} targets))))}
  [state army]
  (:target-map (reduce (fn [result attacker-id]
                         (if (empty? (:targets-left result))
                           result
                           (let [[target-id new-targets-left] (select-target state attacker-id (:targets-left result))]
                             (-> result
                                 (update :target-map assoc attacker-id target-id)
                                 (assoc :targets-left new-targets-left)))))
                       {:target-map {} :targets-left (other-army state army)}
                       (get-groups-in-targeting-order state army))))

(defn get-attack-order
  [state]
  (->> (get-all-groups state)
       (sort-by #(get-im state %))
       (reverse)))


(defn get-target-selection
  [state]
  (let [immune-system-targets (get-targets state :immune-system)
        infection-targets (get-targets state :infection)]
    (into immune-system-targets infection-targets)))





(defn deal-damage
  [state attacker defender]
  (let [damage (get-damage state attacker defender)         ; (get-potential-damage state attacker defender)
        army (get-army-from-group? state defender)
        groups (army state)
        new-groups (reduce (fn [groups group]
                             (if (= (:id group) defender)
                               (let [units-killed (int (/ damage (:hp group)))
                                     new-group (update group :units - units-killed)
                                     units-left (:units new-group)]
                                 (println attacker "did" damage "damage to" defender "and killed" units-killed "units. Left alive:" units-left)
                                 (if (> units-left 0)
                                   (conj groups new-group)
                                   groups))
                               (conj groups group))) [] groups)]
    (assoc state army new-groups)))


(defn attack
  [state attacker defender]
  ;  (println attacker "->" defender)
  (if (and (alive? state attacker) (alive? state defender))
    (deal-damage state attacker defender)
    state))


(defn get-winner
  [{immune-system :immune-system infection :infection}]
  (let [immune-system-units (->> immune-system
                                 (map :units)
                                 (reduce + 0))
        infection-units (->> infection
                             (map :units)
                             (reduce + 0))]
    (cond (not (> immune-system-units 0))
          {:winner :infection :units-left infection-units}

          (not (> infection-units 0))
          {:winner :immune-system :units-left immune-system-units}

          :else
          {:winner :none})))


(defn play-round
  [state]
  (let [targets (get-target-selection state)
        attack-order (get-attack-order state)]
    (loop [current-state state
           remaining-attackers attack-order]
      (if (empty? remaining-attackers)
        current-state
        (let [current-attacker (first remaining-attackers)
              new-state (attack current-state current-attacker (targets current-attacker))]
          (recur new-state (rest remaining-attackers)))))))


(defn play-game
  [state]
  (loop [current-state state]
    (let [{winner :winner :as result} (get-winner current-state)]
      ;(clojure.pprint/pprint current-state)
      (if (not (= :none winner))
        result
        (recur (play-round current-state))))))
