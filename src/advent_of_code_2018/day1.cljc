(ns advent-of-code-2018.day1)

(defn read-input []
  (->> (slurp "resources/input/input-01.txt")
       (clojure.string/split-lines)
       (map read-string)))

(defn get-first-answer []
  (reduce + (read-input)))

(defn get-second-answer []
  (loop [freq-changes (-> (read-input)
                          (repeat)
                          (flatten))
         current-freq 0
         reached-freqs (set [])]
    (if (contains? reached-freqs current-freq)
      current-freq
      (recur (rest freq-changes)
             (+ current-freq (first freq-changes))
             (conj reached-freqs current-freq)))))

