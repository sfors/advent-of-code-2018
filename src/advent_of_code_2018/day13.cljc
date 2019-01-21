(ns advent-of-code-2018.day13
  (:require [clojure.string :as str]
            [clojure.test :refer [is]]))


(defn read-input
  [mode]
  (let [filename (if (= mode :test) "input-13-2.txt" "input-13.txt")]
    (->> (slurp (str "resources/input/" filename))
         (str/split-lines))))


(defn get-location
  [tracks [x y]]
  (-> tracks
      (nth y)
      (nth x)))


(defn get-max-y
  [tracks]
  (dec (count tracks)))


(defn get-max-x
  [tracks]
  (dec (count (first tracks))))


(defn last-pos?
  [tracks [x y]]
  (and (>= x (get-max-x tracks))
       (>= y (get-max-y tracks))))


(defn get-new-coordinates
  [tracks [x y]]
  (if (>= x (get-max-x tracks))
    [0 (inc y)]
    [(inc x) y]))


(defn cart?
  [location]
  (or (= location \<)
      (= location \>)
      (= location \^)
      (= location \v)))


(defn get-cart
  [cart [x y]]
  (let [direction (cond
                    (= cart \<)
                    :left
                    (= cart \>)
                    :right
                    (= cart \^)
                    :up
                    (= cart \v)
                    :down)]
    {:direction direction
     :next-turn :left
     :x         x
     :y         y}))


(defn get-track-from-direction
  [direction]
  (if (or (= direction :left) (= direction :right))
    \-
    \|))


(defn print-tracks
  [tracks]
  (doseq [row tracks] (println "r" row))
  tracks)


(defn get-test-tracks
  []
  ["/->-\\        "
   "|   |  /----\\"
   "| /-+--+-\\  |"
   "| | |  | v  |"
   "\\-+-/  \\-+--/"
   "  \\------/   "]
  )


(defn find-carts
  {:test (fn []
           (let [tracks (get-test-tracks)
                 {new-tracks :tracks carts :carts} (find-carts tracks)]
             ;(print-tracks new-tracks)
             ;(print-tracks tracks)
             ;(println carts)
             ))}
  [tracks]
  (loop [x 0 y 0
         carts '()
         tracks-without-carts []
         current-row []]
    (if (> y (get-max-y tracks))
      {:tracks tracks-without-carts
       :carts  carts}
      (let [location (get-location tracks [x y])
            {cart :cart track :track}
            (if (cart? location)
              (let [cart (get-cart location [x y])
                    track (get-track-from-direction (:direction cart))]
                {:cart cart :track track})
              {:track location})
            current-row (conj current-row track)
            carts (if (nil? cart) carts (cons cart carts))]
        (if (>= x (get-max-x tracks))
          (recur 0 (inc y) carts (conj tracks-without-carts current-row) [])
          (recur (inc x) y carts tracks-without-carts current-row))))))


(defn update-position
  {:test (fn []
           (let [cart {:direction :down, :next-turn :left, :x 12, :y 3}
                 new-cart (update-position cart)]
             (is (= new-cart {:direction :down :next-turn :left :x 12 :y 4}))))}
  [cart]
  (cond
    (= :left (:direction cart))
    (update cart :x dec)
    (= :right (:direction cart))
    (update cart :x inc)
    (= :up (:direction cart))
    (update cart :y dec)
    (= :down (:direction cart))
    (update cart :y inc)))


(defn get-cart-location
  [cart]
  [(:x cart) (:y cart)])


(defn track-turn?
  [location]
  (or (= location \\) (= location \/)))


(defn crossing?
  [location]
  (= location \+))


(defn track-turn-new-direction
  [current-direction location]
  ;(println "curdir, loc" current-direction location)
  (if (= location \\)
    (cond
      (= current-direction :left)
      :up
      (= current-direction :right)
      :down
      (= current-direction :up)
      :left
      (= current-direction :down)
      :right)
    (cond
      (= current-direction :left)
      :down
      (= current-direction :right)
      :up
      (= current-direction :up)
      :right
      (= current-direction :down)
      :left)))


(defn left-hand-turn
  [direction]
  (cond
    (= direction :left)
    :down
    (= direction :right)
    :up
    (= direction :up)
    :left
    (= direction :down)
    :right))


(defn right-hand-turn
  [direction]
  (cond
    (= direction :left)
    :up
    (= direction :right)
    :down
    (= direction :up)
    :right
    (= direction :down)
    :left))

(defn crossing-new-direction
  {:test (fn []
           (let [cart {:direction :down, :next-turn :left, :x 9, :y 4}
                 new-cart (crossing-new-direction cart)]
             (is (= new-cart {:direction :right :next-turn :straight :x 9 :y 4}))))}
  [cart]
  (let [current-direction (:direction cart)
        next-turn (:next-turn cart)]
    (cond
      (= next-turn :straight)
      (assoc cart :next-turn :right)
      (= next-turn :left)
      (-> cart
          (assoc :direction (left-hand-turn current-direction))
          (assoc :next-turn :straight))
      (= next-turn :right)
      (-> cart
          (assoc :direction (right-hand-turn current-direction))
          (assoc :next-turn :left)))))


(defn print-and-return
  [name x]
  (println name x)
  x)



(defn update-direction
  {:test (fn []
           (let [tracks (:tracks (find-carts (get-test-tracks)))
                 cart {:direction :down, :next-turn :left, :x 9, :y 4}
                 new-cart (update-direction cart tracks)]
             (is (= new-cart {:direction :right :next-turn :straight :x 9 :y 4}))))}
  [cart tracks]
  ;(println "updir" cart)
  (let [location (get-location tracks (get-cart-location cart))]
    (cond
      (track-turn? location)
      (update cart :direction track-turn-new-direction location)
      (crossing? location)
      (crossing-new-direction cart)
      :else cart)))




(defn update-cart
  [cart tracks]
  ;(println "Update cart" cart)
  (-> cart
      (update-position)
      (update-direction tracks)))


(defn collide?
  [carts [x y]]
  (some (fn [cart]
          (and (= (:x cart) x) (= (:y cart) y))) carts))


(defn get-all-positions
  [carts]
  (map (fn [cart]
         [(:x cart) (:y cart)]) carts))


(defn get-collisions
  [carts]
  (let [freq (frequencies (get-all-positions carts))
        collisions (->> freq
                        (filter #(> (val %) 1))
                        (map #(first %)))]
    (->> carts
         (filter (fn [cart]
                   (some #(= (get-cart-location cart) %) collisions))))))


(defn sort-carts
  [carts]
  (sort-by (juxt :y :x) carts))


(defn remove-collided
  [carts collided]
  (remove (fn [cart]
            (some? (some #(= cart %) collided))) carts))


(defn find-first-collision
  {:test (fn []
           (let [{tracks :tracks carts :carts} (find-carts (get-test-tracks))
                 {cart :cart collisions :collisions} (find-first-collision tracks carts)]
             (is (= (get-cart-location (first collisions)) [7 3]))))}
  [tracks carts]
  ;(println "max-x" (get-max-x tracks))
  ;(println "max-y" (get-max-y tracks))
  (loop [tick 0
         carts-to-move (sort-carts carts)
         carts-moved []
         collided []]
    (if (and (empty? carts-moved) (empty? carts-to-move))
      {:cart nil :collisions collided}
      (let [cart (update-cart (first carts-to-move) tracks)
            ;coordinates [(:x cart) (:y cart)]
            new-collisions (get-collisions (concat (conj carts-moved cart) carts-to-move))
            new-carts-moved (remove-collided (conj carts-moved cart) new-collisions)
            new-carts-to-move (remove-collided (rest carts-to-move) new-collisions)
            all-collisions (if (not (empty? new-collisions))
                             (concat collided new-collisions)
                             collided)]
        (if (and (empty? carts-moved) (= 1 (count carts-to-move)))
          {:cart cart :collisions all-collisions}
          ;(if (not (empty? new-collisions)))
          ;(collide? (concat (rest carts-to-move) carts-moved) coordinates)
          ;(first new-collisions)
          (if (empty? new-carts-to-move)
            ;(let [collisions (get-collisions (conj carts-moved cart))])
            ; (if (not (empty? collisions))
            ;  collisions
            ;(println "tick" tick "count" (count new-carts-moved))
            (recur (inc tick) (sort-carts new-carts-moved) [] all-collisions)
            (recur tick new-carts-to-move new-carts-moved all-collisions)))))))

(defn get-first-answer
  [mode]
  (let [{tracks :tracks carts :carts} (find-carts (read-input mode))
        first-collision (find-first-collision tracks carts)]
    first-collision
    ))