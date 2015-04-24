(ns algorjthms.core-test
  (:require [clojure.test :refer :all]
            [algorjthms.core :refer :all]))

(deftest kadane-test
  (testing "kadane"
    (is (= 10 (kadane [2 -1 2 3 4 -5])))
    (is (= -1 (kadane [-3 -4 -1 -2]))))
    (is (=  0 (kadane []))))

(deftest quicksort-test
  (testing "quicksort"
    (is (= (range 100) (-> 100 range shuffle quicksort)))))

(deftest lcs-test
  (testing "lcs"
    (is (nil? (lcs nil nil)))
    (is (nil? (lcs '(1 2 3) nil)))
    (is (nil? (lcs nil '(1 2 3))))
    (is (nil? (lcs "rook" "queen")))
    (is (= '(2 3 4) (lcs '(1 2 3 4) '(2 3 4 5))))
    (is (= '(\c \o \o \n) (lcs "comonadic" "raccoon")))))

(deftest hamming-test
  (testing "hamming"
    (is (= 2  (hamming "1011101" "1001001")))
    (is (= 3  (hamming "2173896" "2233796")))
    (is (nil? (hamming [1 2 3] [1 2 3 4])))
    (is (nil? (hamming [1 2 3 4] [1 2 3])))))

(deftest damerau-levenshtein-test
  (testing "damerau-levenshtein"
    (is (= 1   (damerau-levenshtein "clojure" "closure")))
    (is (= 2   (damerau-levenshtein "line" "lein")))
    (is (= 3   (damerau-levenshtein "sitting" "kitten")))
    (is (zero? (damerau-levenshtein "f00b*rZ" "f00b*rZ")))))
