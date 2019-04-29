(ns clobble.core-test
  (:require [clojure.test :refer :all]
            [clobble.core :as core]))

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

  (testing "Final scenarios"
    (testing "Perfect game"
      (is (= {:max-frames 10
              :frames     (conj (vec (repeat 9 strike))
                                [10 10 10])
              :score      300}
             (tally-frames (conj (vec (repeat 9 strike))
                                 [10 10 10])))))
    (testing "Almost perfect game"
      (is (= {:max-frames 10
              :frames     (conj (vec (repeat 9 strike))
                                [10 10 9])
              :score      299}
             (tally-frames (conj (vec (repeat 9 strike))
                                 [10 10 9])))))
    (testing "Botched the last roll"
      (is (= {:max-frames 10
              :frames     (conj (vec (repeat 9 strike))
                                [10 10 0])
              :score      290}
             (tally-frames (conj (vec (repeat 9 strike))
                                 [10 10 0])))))
    (testing "Disappointing last frame"
      (is (= {:max-frames 10
              :frames     (conj (vec (repeat 9 strike))
                                [0 0])
              :score      240}
             (tally-frames (conj (vec (repeat 9 strike))
                                 [0 0])))))
    (testing "So so game"
      (is (= {:max-frames 10
              :frames     (repeat 10 not-special)
              :score      50}
             (tally-frames (repeat 10 not-special)))))))

(deftest tally-illegal-frame-into-card
  (testing "Card already finished"
    (is (thrown? AssertionError (tally-frames (repeat 11 not-special)))))

  (testing "Non final final-with-bonus scenario"
    (is (thrown? AssertionError (tally-frames [[10 10 10]]))))

  (testing "Unacceptable frame"
    (is (thrown? AssertionError (tally-frames [[7 7]])))
    (is (thrown? AssertionError (tally-frames [[7 7]])))
    (is (thrown? AssertionError (tally-frames [[1 2 3]])))
    (is (thrown? AssertionError (tally-frames [[7 3 3]])))
    (is (thrown? AssertionError (tally-frames [[10 3 3]])))
    (is (thrown? AssertionError (tally-frames [[1 2 3 4]])))
    (is (thrown? AssertionError (tally-frames [[1 2 3 4 5]])))
    (is (thrown? AssertionError (tally-frames (conj (vec (repeat 9 strike))
                                                    [10 10 10 10]))))
    (is (thrown? AssertionError (tally-frames (conj (vec (repeat 9 strike))
                                                    [10 10]))))
    (is (thrown? AssertionError (tally-frames (conj (vec (repeat 9 strike))
                                                    [10 10 12]))))))