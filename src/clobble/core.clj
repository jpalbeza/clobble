(ns clobble.core
  (:require [clobble.frame]))

(defn empty-score-card
  []
  {:max-frames 10
   :frames     []
   :score      0})

(defn is-finished?
  [{:keys [max-frames frames]}]
  (>= (count frames) max-frames))

(defn- score
  [{:keys [max-frames frames] :as card}]
  (assoc card :score (->> (range max-frames)
                          (map #(drop % frames))
                          (take-while (comp not empty?))
                          (map clobble.frame/->numeric-score)
                          (take-while number?)
                          (reduce +))))

(defn- is-1-frame-to-go
  [{:keys [max-frames frames]}]
  (= max-frames (+ 1 (count frames))))

(defn- is-valid-frame?
  [card frame]
  (let [named-score (apply clobble.frame/->named-score frame)]
    (or (and (is-1-frame-to-go card)
             (clobble.frame/final? named-score))
        (clobble.frame/non-final? named-score))))

(defn tally-frame
  [card & frame]
  {:pre [(not (is-finished? card))
         (is-valid-frame? card frame)]}
  (-> card
      (update :frames conj frame)
      score))
