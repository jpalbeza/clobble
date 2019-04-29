(ns clobble.core-test
  (:require [clojure.test :refer :all]
            [clobble.core :as core]))

(deftest single-frame
  (testing "Single frame name"
    (is (= :strike (core/->named-frame-score 10)))
    (is (= :spare (core/->named-frame-score 3 7)))
    (is (= :not-special (core/->named-frame-score 2 7)))
    (is (nil? (core/->named-frame-score 1)) "Failed test on illegal frame")
    (is (nil? (core/->named-frame-score 1 2 3)) "Failed test on illegal frame"))

  (testing "Strike score"
    (is (= :incalculable (core/->numeric-frame-score [[10]])))
    (is (= 15 (core/->numeric-frame-score [[10] [2 3]])))
    (is (= 30 (core/->numeric-frame-score [[10] [10] [10]]))))

  (testing "Spare score"
    (is (= :incalculable (core/->numeric-frame-score [[3 7]])))
    (is (= 14 (core/->numeric-frame-score [[3 7] [4 5]])))
    (is (= 20 (core/->numeric-frame-score [[3 7] [10]])))))

(def empty-card (core/empty-score-card))

(defn- tally-frames
  [frames]
  (->> frames
       (reduce #(apply core/tally-frame %1 %2) empty-card)))

(def strike [10])
(def spare [7 3])
(def not-special [2 3])

(deftest tally-frame-into-card
  (testing "Strike scenario"
    (is (= {:max-frames 10
            :frames     [strike]
            :score      0}
           (tally-frames [strike])))
    (is (= {:max-frames 10
            :frames     (repeat 2 strike)
            :score      0}
           (tally-frames (repeat 2 strike))))
    (is (= {:max-frames 10
            :frames     (repeat 3 strike)
            :score      30}
           (tally-frames (repeat 3 strike)))))

  (testing "Spare scenario"
    (is (= {:max-frames 10
            :frames     [spare]
            :score      0}
           (tally-frames [spare])))
    (is (= {:max-frames 10
            :frames     (repeat 2 spare)
            :score      17}
           (tally-frames (repeat 2 spare))))
    (is (= {:max-frames 10
            :frames     (repeat 3 spare)
            :score      34}
           (tally-frames (repeat 3 spare)))))

  (testing "Not special scenario"
    (is (= {:max-frames 10
            :frames     [not-special]
            :score      5}
           (tally-frames [not-special])))
    (is (= {:max-frames 10
            :frames     (repeat 2 not-special)
            :score      10}
           (tally-frames (repeat 2 not-special)))))

  (testing "Card already finished"
    (is (thrown? AssertionError (tally-frames (repeat 11 not-special))))))