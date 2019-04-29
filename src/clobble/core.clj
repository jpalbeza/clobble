(ns clobble.core)

(defn empty-score-card
  []
  {:max-frames 10
   :frames     []
   :score      0})

(defn is-finished?
  [{:keys [max-frames frames]}]
  (>= (count frames) max-frames))

(defn ->named-frame-score
  ([r] (when (= r 10) :strike))
  ([r1 r2] (when (every? pos? [r1 r2])
             (if (= 10 (+ r1 r2))
               :spare
               :not-special)))
  ([r1 r2 r3] (when (or (= :strike (->named-frame-score r1))
                        (= :spare (->named-frame-score r1 r2)))
                (when (pos? r3)
                  :final-with-bonus)))
  ;([_ _ _ _ & _] nil)
  )

(defn ->strike-score
  [remaining-frames]
  (let [next-2-rolls (->> remaining-frames
                          flatten
                          (take 2))]
    (if (and (= 2 (count next-2-rolls))
             (every? number? next-2-rolls))
      (->> (conj next-2-rolls 10)
           (reduce +))
      :incalculable)))

(defn ->spare-score
  [remaining-frames]
  (let [next-roll (->> remaining-frames
                       flatten
                       first)]
    (if (number? next-roll)
      (+ 10 next-roll)
      :incalculable)))

(defn ->numeric-frame-score
  [[current-frame & remaining-frames]]
  (let [named-score (apply ->named-frame-score current-frame)]
    (case named-score
      :strike (->strike-score remaining-frames)
      :spare (->spare-score remaining-frames)
      :not-special (reduce + current-frame)
      ;; TODO :final-with-bonus (reduce + current-frame)
      )))

(defn score
  [{:keys [max-frames frames] :as card}]
  (assoc card :score (->> (range max-frames)
                          (map #(drop % frames))
                          (take-while (comp not empty?))
                          (map ->numeric-frame-score)
                          (take-while number?)
                          (reduce +))))

(defn tally-frame
  [card & frame]
  {:pre [(not (is-finished? card))]}
  (-> card
      (update :frames conj frame)
      score))
