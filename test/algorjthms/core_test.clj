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

(deftest euclidean-distance-test
  (testing "euclidean-distance"
   (is (= 5
          (euclidean-distance [2 -1] [-2 2])))
   (is (= 5.0990195135927845
          (euclidean-distance [2 3 6] [5 4 10])))
   (is (= 9.643650760992955
          (euclidean-distance [1 2 3 5 8 13 21] [0 2 4 6 8 10 12])))))

(deftest manhattan-distance-test
  (testing "manhattan-distance"
    (is (= 0 (manhattan-distance [0 0] [0 0])))
    (is (= 4 (manhattan-distance [1 2] [3 0])))
    (is (= 6 (manhattan-distance [0 0] [3 3])))))

(deftest power-mod-test
  (testing "power-mod"
    (is (= 1527229998585248450016808958343740453059
           (power-mod 2988348162058574136915891421498819466320163312926952423791023078876139
                      2351399303373464486466122544523690094744975233415544072992656881240319
                      10000000000000000000000000000000000000000)))))

(deftest lcm-test
  (testing "lcm"
    (is (= 36 (lcm 12 18)))
    (is (= 42 (lcm 14 -6)))
    (is (= 0  (lcm 0  35)))))

(deftest extended-gcd-test
  (testing "extended-gcd"
    (is (= [-9  47] (extended-gcd  120  23)))
    (is (= [-3   5] (extended-gcd   65  40)))
    (is (= [-16 27] (extended-gcd 1239 735)))))

(deftest modular-inverse-test
  (testing "modular-inverse"
    (is (= 1969 (modular-inverse 42  2017)))
    (is (thrown? Exception (modular-inverse 15 60)))))

(deftest roots-test
  (testing "roots"
    (is (= '(-9.381755897326649E-14 0.9999999999998124 1.9999999999997022)
           (roots #(+ (* % % %) (* -3 % %) (* 2 %)) -1.0 3.0 0.0001 0.00000001)))))

(deftest neighbors-test
  (testing "neighbors"
    (let [matrix [[1 2 3] [4 5 6] [7 8 9]]]
      (is (= '(4 2) (map #(get-in matrix %) (neighbors 3 [0 0])))))))

(deftest estimate-cost-test
  (testing "estimate-cost"
    (is (= 7200 (estimate-cost 900 5 0 0)))
    (is (=    0 (estimate-cost 900 5 4 4)))))

(deftest path-cost-test
  (testing "path-cost"
    (is (= 901 (path-cost 900 {:cost 1})))))

(deftest total-cost-test
  (testing "total-cost"
    (is (= 7200 (total-cost    0 900 5 0 0)))
    (is (= 1900 (total-cost 1000 900 5 3 4)))))
