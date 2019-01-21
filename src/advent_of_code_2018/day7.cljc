(ns advent-of-code-2018.day7
  (:require [clojure.string :as str]
            [clojure.test :refer [is]]))


(defn read-input
  [mode]
  (let [path (if (= mode :test)
               "resources/input/input-07-2.txt"
               "resources/input/input-07.txt"
               )]
    (->>
      (slurp path)
      (str/split-lines)
      (map (fn [s]
             (->>
               (re-seq #" \w{1} " s)
               (map str/trim)))))))


(def get-times
  (->> "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
       (map-indexed (fn [index step]
                      [(str step) (+ 0 (inc index))]))
       (into {})))


(defn get-time
  [step]
  (get get-times step))


(defn get-dependencies
  [dependency-list]
  (reduce (fn [result [depends-on step]]
            (if (get result step)
              (update result step conj depends-on)
              (assoc result step [depends-on]))) {} dependency-list))


(defn get-all-steps
  [dependency-list]
  (-> (flatten dependency-list)
      (distinct)
      (sort)))


(defn get-available-steps
  (
   [steps dependencies done]
   (get-available-steps steps dependencies done []))
  ([steps dependencies done in-progress]
   (->>
     (remove (fn [step]
               (or (some #(= step %) done)
                   (some #(= step %) in-progress))) steps)
     (filter (fn [step]
               (every? (fn [dependency]
                         (some #(= dependency %) done)) (get dependencies step))))
     (sort))))


(defn get-first-answer
  [input]
  (->>
    (let [steps (get-all-steps input)
          dependencies (get-dependencies input)]
      (loop [done []
             available-steps (get-available-steps steps dependencies done)]
        (if (= (count steps) (count done))
          done
          (let [new-done (conj done (first available-steps))
                ]
            (recur new-done (get-available-steps steps dependencies new-done))))))
    (apply str)))


(def get-worker
  {:working-on nil :time-left 0})


(defn get-workers
  [n]
  (repeat n get-worker))


(defn get-steps-in-progress
  [workers]
  (reduce (fn [in-progress {working-on :working-on time-left :time-left}]
            (if (or (nil? working-on) (> 1 time-left))
              in-progress
              (conj in-progress working-on))) [] workers))


(defn get-completed-steps
  [workers]
  (reduce (fn [completed {working-on :working-on time-left :time-left}]
            (if (or (nil? working-on) (<= 1 time-left))
              completed
              (conj completed working-on))) [] workers))


(defn available?
  {:test (fn []
           (is (not (available? {:working-on "C" :time-left 5})))
           (is (available? {:working-on "C" :time-left 0}))
           )}
  [{working-on :working-on time-left :time-left}]
  (or (nil? working-on) (> 1 time-left)))


(defn assign-step
  [worker step]
  (let [time (if (nil? step) 0 (dec (get-time step)))]
    (-> worker
        (assoc :working-on step)
        (assoc :time-left time))))


(defn decrease-time-left
  [worker]
  (update worker :time-left dec))


(defn update-workers
  [workers available-steps]
  (->
    (reduce (fn [{workers :workers available-steps :available-steps}
                 worker]
              (if (available? worker)
                {:workers         (conj workers (assign-step worker (first available-steps)))
                 :available-steps (rest available-steps)}
                {:workers         (conj workers (decrease-time-left worker))
                 :available-steps available-steps}))
            {:workers [] :available-steps available-steps} workers)
    :workers))


(defn get-second-answer
  [input]
  (->>
    (let [steps (get-all-steps input)
          dependencies (get-dependencies input)]
      (loop [sec 0
             workers (get-workers 2)
             done []]
        (if (= (count steps) (count done))
          (dec sec)
          (let [in-progress (get-steps-in-progress workers)
                done (concat done (get-completed-steps workers))
                available-steps (get-available-steps steps dependencies done in-progress)
                workers (update-workers workers available-steps)]
            (println "Second" sec)
            (println "In progress" (get-steps-in-progress workers))
            (println "Done" done)
            (println "Workers" workers)
            (recur (inc sec) workers done)))))))


