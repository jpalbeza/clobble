(ns clobble.frame-test
  (:require [clojure.test :refer :all]))

(deftest single-frame
  (testing "Single frame name"
    (is (= :strike (clobble.frame/->named-score 10)))
    (is (= :spare (clobble.frame/->named-score 3 7)))
    (is (= :not-special (clobble.frame/->named-score 2 7)))
    (is (nil? (clobble.frame/->named-score 1)) "Failed test on illegal frame")
    (is (nil? (clobble.frame/->named-score 1 2 3)) "Failed test on illegal frame"))

  (testing "Strike score"
    (is (= :incalculable (clobble.frame/->numeric-score [[10]])))
    (is (= 15 (clobble.frame/->numeric-score [[10] [2 3]])))
    (is (= 30 (clobble.frame/->numeric-score [[10] [10] [10]]))))

  (testing "Spare score"
    (is (= :incalculable (clobble.frame/->numeric-score [[3 7]])))
    (is (= 14 (clobble.frame/->numeric-score [[3 7] [4 5]])))
    (is (= 20 (clobble.frame/->numeric-score [[3 7] [10]])))))
