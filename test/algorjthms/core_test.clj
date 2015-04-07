(ns algorjthms.core-test
  (:require [clojure.test :refer :all]
            [algorjthms.core :refer :all]))

(deftest kadane-test
  (testing "kadane"
    (is (= 10 (kadane [2 -1 2 3 4 -5])))
    (is (= -1 (kadane [-3 -4 -1 -2]))))
    (is (=  0 (kadane []))))
