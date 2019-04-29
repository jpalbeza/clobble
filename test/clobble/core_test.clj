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
