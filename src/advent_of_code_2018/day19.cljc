(ns advent-of-code-2018.day19
  (:require [clojure.string :as str]
            [clojure.test :refer [is]]
            [advent-of-code-2018.day16 :refer [operation-map]]))


(defn read-program
  [filepath]
  (let [text (-> (slurp filepath)
                 (str/split-lines))
        ip-binding (-> (first text)
                       (str/split #" ")
                       (second)
                       (read-string))
        instructions (->> (rest text)
                          (map (fn [line]
                                 (let [[instr a b c] (str/split line #" ")]
                                   {:op (keyword instr)
                                    :a  (read-string a)
                                    :b  (read-string b)
                                    :c  (read-string c)}))))]
    {:ip-binding   ip-binding
     :instructions instructions
     :length       (count instructions)}))


(defn run-program
  [program start-registers]
  (loop [ip 0
         registers start-registers
         tick 0]
    (cond
      (>= ip (:length program))
      {:registers registers :tick tick :status :normal}

      (> tick 50000000)
      {:registers registers :tick tick :status :stopped}

      :else
      (let [{op :op a :a b :b c :c} (nth (:instructions program) ip)
            operation (operation-map op)
            post-registers (-> registers
                               (assoc (:ip-binding program) ip)
                               (operation a b c))
            new-ip (inc (nth post-registers (:ip-binding program)))]
        (when (= 0 (mod tick 10000000))
          (println "post-registers" post-registers "ip" new-ip "tick" tick))
        (recur new-ip post-registers (inc tick))))))

(defn get-first-answer
  []
  (let [program (read-program "resources/input/input-19.txt")
        post-registers (first (run-program program [1 0 0 0 0 0]))]
    (nth post-registers 0)))