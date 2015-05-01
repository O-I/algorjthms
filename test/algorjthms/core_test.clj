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

(deftest binary-search-test
  (testing "binary-search"
    (let [coll [1 3 4 6 8 9 11]]
      (is (= 3  (binary-search coll 6)))
      (is (= 5  (binary-search coll 9)))
      (is (nil? (binary-search coll 7)))
      (is (= 4  (binary-search coll 8 1 5)))
      (is (nil? (binary-search coll 8 5 6)))
      (is (nil? (binary-search coll 8 5 1))))))

(deftest gcd-test
  (testing "gcd"
    (is (= 9973 (gcd 49865 69811)))
    (is (= 21   (gcd 1071 1029)))
    (is (= 11   (gcd 11 0)))
    (is (= 11   (gcd 0 11)))
    (is (= 1    (gcd 32416190071 541)))))

(deftest merge-sort-test
  (testing "merge-sort"
    (is (= (range 100) (-> 100 range shuffle quicksort)))))
