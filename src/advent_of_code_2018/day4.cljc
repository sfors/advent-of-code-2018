(ns advent-of-code-2018.day4
  (:require [clojure.string :as str]
            [clojure.test :refer [is]]))


(defn get-status-or-id-from-string
  {:test (fn []
           (is (= :awake (get-status-or-id-from-string "wakes up")))
           (is (= :asleep (get-status-or-id-from-string "falls asleep")))
           (is (= "1229" (get-status-or-id-from-string "Guard #1229 begins shift"))))}
  [status-string]
  (let [[status id] (str/split status-string #" " 3)]
    (cond
      (= status "wakes")
      :awake
      (= status "falls")
      :asleep
      (= status "Guard")
      (subs id 1))))


(defn id?
  [status-or-id]
  (string? status-or-id))


(defn get-details-from-line
  [line]
  (let [[date-string status-string] (str/split line #"] ")
        date (subs date-string 1)
        status-or-id (get-status-or-id-from-string status-string)]
    (assoc (if (id? status-or-id)
             {:id status-or-id :status :new-guard}
             {:status status-or-id}) :date date)))


(defn add-missing-ids
  [events]
  (->> events
       (reduce (fn [{history :history current-guard :current-guard} event]
                 (let [id (:id event current-guard)
                       event-with-id (assoc event :id id)]
                   {:current-guard id
                    :history       (conj history event-with-id)}))
               {:current-guard :none
                :history       []})
       (:history)))


(defn minute
  {:test (fn []
           (is (= 59 (minute "1518-11-23 00:59")))
           (is (= 8 (minute "1518-02-05 00:08")))
           )}
  [date-string]
  (->
    (str/split date-string #":")
    (second)
    (Integer/parseInt)))


(defn hour
  {:test (fn []
           (is (= 00 (hour "1518-11-23 00:59"))))}
  [date-string]
  (-> (str/split date-string #" ")
      (second)
      (subs 0 2)
      (Integer/parseInt)))


(defn read-input
  []
  (->> (slurp "resources/input/input-04.txt")
       (clojure.string/split-lines)
       (map get-details-from-line)
       (sort-by :date)
       (add-missing-ids)))


(defn awake?
  [status]
  (or (= status :new-guard) (= status :awake)))


(defn add-minutes-to-guard
  [guards id status minutes]
  (let [status (if (= status :new-guard) :awake status)]
    (if (get guards id)
      (update-in guards [id status] concat minutes)
      (assoc guards id {status minutes}))))


(defn get-guards-with-minutes
  []
  (loop [events (read-input)
         guards {}]
    (if (empty? events)
      guards
      (let [current-event (first events)
            next-event (second events)
            current-guard-id (:id current-event)
            current-status (:status current-event)
            next-status (:status next-event)
            from-time (if (= current-status :new-guard) 0 (minute (:date current-event)))
            to-time (if (or (nil? next-status) (= next-status :new-guard)) 60 (minute (:date next-event)))]
        (recur (rest events)
               (add-minutes-to-guard guards current-guard-id current-status (range from-time to-time)))))))


(defn set-sleep-stats
  [guard]
  (if (:asleep guard)
    (let [sum (reduce + (:asleep guard))
          [minute freq] (->> (:asleep guard)
                             (frequencies)
                             (reduce (fn [[max-minute max-freq] [minute freq]]
                                       (if (> freq max-freq)
                                         [minute freq]
                                         [max-minute max-freq]))))]

      {:sum sum :minute minute :frequency freq})
    {:sum 0 :minute 0 :frequency 0}))


(defn get-guards
  []
  (->> (get-guards-with-minutes)
       (map (fn [[id guard]]
              (assoc guard :id id)))
       (map (fn [guard]
              (assoc guard :sleep-stats (set-sleep-stats guard))))))


(defn get-total-minutes-slept
  [guard]
  (get-in guard [:sleep-stats :sum]))


(defn get-most-frequent-minute
  [guard]
  (get-in guard [:sleep-stats :minute]))


(defn get-highest-frequency
  [guard]
  (get-in guard [:sleep-stats :frequency]))

(defn get-most-sleepy-guard
  []
  (->> (get-guards)
       (reduce (fn [most-sleepy-guard guard]
                 (if (> (get-total-minutes-slept guard) (get-total-minutes-slept most-sleepy-guard))
                   guard
                   most-sleepy-guard)))))


(defn get-most-sleepy-guard-2
  []
  (->> (get-guards)
       (reduce (fn [most-sleepy-guard guard]
                 (if (> (get-highest-frequency guard) (get-highest-frequency most-sleepy-guard))
                   guard
                   most-sleepy-guard)))))


(defn get-first-answer
  []
  (let [guard (get-most-sleepy-guard)
        id-number (Integer/parseInt (:id guard))
        minute (get-in guard [:sleep-stats :minute])]
    (* id-number minute)))

(defn get-second-answer
  []
  (let [guard (get-most-sleepy-guard-2)
        id-number (Integer/parseInt (:id guard))
        minute (get-in guard [:sleep-stats :minute])]
    (* id-number minute)))
