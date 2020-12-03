(defn load-input-txt [path]
  (->> path
       (slurp)
       (.trim)
       (#(.split % "\n"))
       (map #(Integer/parseInt %))))

;(def xs [20 200 1800 2000])
(def xs (load-input-txt "input.txt"))

(defn find-partner
  [x xs]
  (cond
    (= 2020 (+ x (first xs))) [x (first xs) (rest xs)]
    (seq (rest xs)) (recur x (rest xs))))

(defn find-2020-pair
  "Return a pair of numbers in xs that add up to 2020"
  [xs]
  (let [pair (find-partner (first xs) (rest xs))]
    (cond
      (vector? pair) pair
      (seq (rest xs)) (recur (rest xs)))))

(let [pair (find-2020-pair xs)]
  (* (first pair) (second pair)))
