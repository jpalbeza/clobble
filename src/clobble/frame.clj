(ns clobble.frame)

(defn- is-a-valid-roll-score?
  [n]
  (and (number? n)
       (<= 0 n 10)))

(defn ->named-score
  ([r] (when (= r 10) :strike))
  ([r1 r2] (when (every? is-a-valid-roll-score? [r1 r2])
             (let [roll-sum (+ r1 r2)]
               (cond (= 10 roll-sum) :spare
                     (> 10 roll-sum) :not-special))))
  ([r1 r2 r3] (when (or (= :strike (->named-score r1))
                        (= :spare (->named-score r1 r2)))
                (when (is-a-valid-roll-score? r3)
                  :final-with-bonus)))
  ([_ _ _ _ & _] nil))

(defn- ->strike-score
  [remaining-frames]
  (let [next-2-rolls (->> remaining-frames
                          flatten
                          (take 2))]
    (if (and (= 2 (count next-2-rolls))
             (every? number? next-2-rolls))
      (->> (conj next-2-rolls 10)
           (reduce +))
      :incalculable)))

(defn- ->spare-score
  [remaining-frames]
  (let [next-roll (->> remaining-frames
                       flatten
                       first)]
    (if (number? next-roll)
      (+ 10 next-roll)
      :incalculable)))

(defn ->numeric-score
  [[current-frame & remaining-frames]]
  (let [named-score (apply clobble.frame/->named-score current-frame)]
    (case named-score
      :strike (->strike-score remaining-frames)
      :spare (->spare-score remaining-frames)
      :not-special (reduce + current-frame)
      :final-with-bonus (reduce + current-frame))))


(def non-final? #{:strike :spare :not-special})
(def final? #{:not-special :final-with-bonus})