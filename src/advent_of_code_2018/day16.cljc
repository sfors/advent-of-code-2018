(ns advent-of-code-2018.day16
  (:require [clojure.string :as str]
            [clojure.test :refer [is]]
            [clojure.string :as str]))


(defn read-input
  []
  (let [lines (->> (slurp (str "resources/input/input-16.txt"))
                   (str/split-lines))]
    (loop [[line1 line2 line3 _ & remaining] lines
           result {:examples []}]
      (let [before (read-string (second (str/split line1 #": " 2)))
            [opcode a b c] (map #(Integer/parseInt %) (str/split line2 #" "))
            after (read-string (second (str/split line3 #": " 2)))
            example {:before    before
                     :after     after
                     :operation {:opcode opcode
                                 :a      a
                                 :b      b
                                 :c      c}}
            new-result (update result :examples conj example)]
        (if (empty? (first remaining))
          (let [program (rest (rest remaining))]
            (->> (map (fn [row]
                        (->> (str/split row #" ")
                             (map #(Integer/parseInt %)))) program)
                 (assoc new-result :program)))
          (recur remaining new-result))))))


(defn addr
  {:test (fn []
           (is (= [1 1 2 3] (addr [1 1 2 1] 1 2 3))))}
  [registers a b c]
  (let [result (+ (nth registers a) (nth registers b))]
    (assoc registers c result)))


(defn addi
  {:test (fn []
           (is (= [6 1 2 1] (addi [1 1 2 1] 2 4 0))))}
  [registers a b c]
  (let [result (+ (nth registers a) b)]
    (assoc registers c result)))


(defn mulr
  {:test (fn []
           (is (= [3 2 2 1] (mulr [3 2 1 1] 2 1 2))))}
  [registers a b c]
  (let [result (* (nth registers a) (nth registers b))]
    (assoc registers c result)))


(defn muli
  [registers a b c]
  (let [result (* (nth registers a) b)]
    (assoc registers c result)))


(defn banr
  [registers a b c]
  (let [result (bit-and (nth registers a) (nth registers b))]
    (assoc registers c result)))


(defn bani
  [registers a b c]
  (let [result (bit-and (nth registers a) b)]
    (assoc registers c result)))

(defn borr
  [registers a b c]
  (let [result (bit-or (nth registers a) (nth registers b))]
    (assoc registers c result)))


(defn bori
  [registers a b c]
  (let [result (bit-or (nth registers a) b)]
    (assoc registers c result)))


(defn setr
  [registers a _ c]
  (let [result (nth registers a)]
    (assoc registers c result)))


(defn seti
  {:test (fn []
           (is (= [3 2 2 1] (seti [3 2 1 1] 2 1 2))))}
  [registers a _ c]
  (assoc registers c a))


(defn gtir
  [registers a b c]
  (let [result (if (> a (nth registers b)) 1 0)]
    (assoc registers c result)))


(defn gtri
  [registers a b c]
  (let [result (if (> (nth registers a) b) 1 0)]
    (assoc registers c result)))


(defn gtrr
  [registers a b c]
  (let [result (if (> (nth registers a) (nth registers b)) 1 0)]
    (assoc registers c result)))


(defn eqir
  [registers a b c]
  (let [result (if (= a (nth registers b)) 1 0)]
    (assoc registers c result)))


(defn eqri
  [registers a b c]
  (let [result (if (= (nth registers a) b) 1 0)]
    (assoc registers c result)))


(defn eqrr
  [registers a b c]
  (let [result (if (= (nth registers a) (nth registers b)) 1 0)]
    (assoc registers c result)))


(defn apply-operations
  [registers a b c]
  ((juxt addr addi mulr muli banr bani borr bori setr seti gtir gtri gtrr eqir eqri eqrr) registers a b c))


(defn get-first-answer
  []
  (let [{examples :examples} (read-input)]
    (->> (map (fn [{before :before after :after {a :a b :b c :c} :operation}]
                (let [applied-operations (apply-operations before a b c)
                      matching (filter #(= after %) applied-operations)
                      number-matching (count matching)]
                  number-matching
                  )) examples)
         (filter #(>= % 3))
         (count))))


(defn get-all-with-opcode
  [examples opcode]
  (filter (fn [example]
            (= opcode (get-in example [:operation :opcode]))) examples))


(defn match?
  [{before :before after :after {a :a b :b c :c} :operation} instruction]
  (= after (instruction before a b c)))


(defn match-all-operations
  [examples instruction]
  (every? #(match? % instruction) examples))


(def operation-map
  {:addr addr
   :addi addi
   :mulr mulr
   :muli muli
   :banr banr
   :bani bani
   :borr borr
   :bori bori
   :setr setr
   :seti seti
   :gtir gtir
   :gtri gtri
   :gtrr gtrr
   :eqir eqir
   :eqri eqri
   :eqrr eqrr})


(defn get-candidates
  [{examples :examples}]
  (map (fn [opcode]
         (let [examples-with-opcode (get-all-with-opcode examples opcode)
               matching-operations (filter #(match-all-operations examples-with-opcode (val %)) operation-map)]
           {:opcode opcode :operations (map key matching-operations)}
           )) (range 0 16)))


(defn in?
  [coll element]
  (some #(= element %) coll))


(defn get-operation-opcode-map
  [input]
  (loop [done {}
         candidates (get-candidates input)]
    ;(println "done" done)
    ;(println "candidates" candidates)
    (if (empty? candidates)
      done
      (let [singles (filter #(= 1 (count (:operations %))) candidates)
            single-mapping (map (fn [{opcode :opcode [operation] :operations}]
                                  {:opcode opcode :operation operation}) singles)
            new-done (reduce (fn [result {opcode :opcode operation :operation}]
                               (assoc result opcode operation)) done single-mapping)
            finished-operations (map val new-done)
            new-candidates (reduce (fn [result {opcode :opcode operations :operations}]
                                     (if (not (some? (get new-done opcode)))
                                       (let [filtered-operations (filter #(not (in? finished-operations %)) operations)]
                                         (conj result {:opcode opcode :operations filtered-operations}))
                                       result)) [] candidates)]
        ;(println "singles" singles)
        ;(println "single-mapping" single-mapping)
        ;(println "new-done" new-done)
        ;(println "fin" finished-operations)
        ;(println "new can" new-candidates)
        (recur new-done new-candidates)))))



(defn run-program
  []
  (let [input (read-input)
        operation-mapping (get-operation-opcode-map input)
        program (:program input)]
    (reduce (fn [registers [opcode a b c]]
              (let [instruction (get operation-map (get operation-mapping opcode))]
                (instruction registers a b c))) [0 0 0 0] program)))
