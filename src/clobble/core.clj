(ns clobble.core)

(defn empty-score-card
  []
  {:max-frames 10
   :frames     []
   :score      0})

(defn is-finished?
  [{:keys [max-frames frames]}]
  (>= (count frames) max-frames))

(defn is-a-number-greater-than-equal-to-zero
  [n]
  (and (number? n)
       (<= 0 n)))

(defn ->named-frame-score
  ([r] (when (= r 10) :strike))
  ([r1 r2] (when (every? is-a-number-greater-than-equal-to-zero [r1 r2])
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
      :final-with-bonus (reduce + current-frame))))

(defn score
  [{:keys [max-frames frames] :as card}]
  (assoc card :score (->> (range max-frames)
                          (map #(drop % frames))
                          (take-while (comp not empty?))
                          (map ->numeric-frame-score)
                          (take-while number?)
                          (reduce +))))

(def non-final-frames? #{:strike :spare :not-special})
(def final-frames? #{:not-special :final-with-bonus})

(defn- is-1-frame-to-go
  [{:keys [max-frames frames]}]
  (= max-frames (+ 1 (count frames))))

(defn- is-valid-frame?
  [card frame]
  (let [named-score (apply ->named-frame-score frame)]
    (or (and (is-1-frame-to-go card)
             (final-frames? named-score))
        (non-final-frames? named-score))))

(defn tally-frame
  [card & frame]
  {:pre [(not (is-finished? card))
         (is-valid-frame? card frame )]}
  (-> card
      (update :frames conj frame)
      score))
