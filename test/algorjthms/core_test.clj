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

(deftest swap-test
  (testing "swap"
    (is (= [1 2 4 3] (swap [1 2 3 4] 2 3)))))

(deftest knuth-shuffle-test
  (testing "knuth-shuffle"
    (is (not= (-> 100 range vec) (knuth-shuffle (-> 100 range vec))))))

(deftest neighbors-test
  (testing "neighbors"
    (let [matrix [[1 2 3] [4 5 6] [7 8 9]]]
      (is (= [4 2] (map #(get-in matrix %) (neighbors 3 [0 0])))))))

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

(deftest min-by-test
  (testing "min-by"
    (is (= {:cost 9} (min-by :cost [{:cost 100} {:cost 36} {:cost 9}])))))

(deftest A*-test
  (testing "A*"
    (let [world1 [[  1   1   1   1   1]
                  [999 999 999 999   1]
                  [  1   1   1   1   1]
                  [  1 999 999 999 999]
                  [  1   1   1   1   1]]
          world2 [[  1   1   1   2   1]
                  [  1   1   1 999   1]
                  [  1   1   1 999   1]
                  [  1   1   1 999   1]
                  [  1   1   1   1   1]]
          world3 [[  1   1   1   2   1]
                  [  1   1   1 999   1]
                  [  1   1   1 999   1]
                  [  1   1   1 999   1]
                  [  1   1   1 666   1]]]
      (is (= [{:cost 17,
               :yxs [[0 0] [0 1] [0 2] [0 3] [0 4]
                     [1 4] [2 4] [2 3] [2 2] [2 1]
                     [2 0] [3 0] [4 0] [4 1] [4 2]
                     [4 3] [4 4]]}
              :steps 94]
             (A* [0 0] 900 world1)))
      (is (= [{:cost 9,
               :yxs [[0 0] [0 1] [0 2] [1 2] [2 2]
                     [3 2] [4 2] [4 3] [4 4]]}
               :steps 134]
             (A* [0 0] 900 world2)))
      (is (= [{:cost 10,
               :yxs [[0 0] [0 1] [0 2] [0 3] [0 4]
                     [1 4] [2 4] [3 4] [4 4]]}
               :steps 132]
             (A* [0 0] 900 world3))))))

(deftest position-test
  (testing "position"
    (is (= '(5 7)   (position #{3 4} [:a 1 :b 2 :c 3 :d 4])))
    (is (= '(:c :d) (position #{3 4} {:a 1 :b 2 :c 3 :d 4})))
    (is (= '(0 2)   (position even? '(2 3 6 7))))))

(deftest horner-test
  (testing "horner"
    (is (= 128 (horner [-19 7 -4 6] 3)))))

(deftest aks?-test
  (testing "aks?"
    (is (= '(2 3 5 7 11 13 17 19 23 29 31 37 41 43 47)
           (filter aks? (range 2 50))))))

(deftest digit-count-test
  (testing "digit-count"
    (is (= 1 (digit-count 0)))
    (is (= 3 (digit-count -123)))
    (is (= 5 (digit-count 24159)))))

(deftest choose-test
  (testing "chooose"
    (is (= '(1 5 10 10 5 1)
           (map (fn [[n k]] (choose n k)) (for [i (range 6)] [5 i]))))))

(deftest lempel-ziv-welch-test
  (testing "lempel-ziv-welch"
    (is (= '(84 79 66 69 79 82 78 79 84 256 258 260 265 259 261 263)
           (lempel-ziv-welch "TOBEORNOTTOBEORTOBEORNOT")))))

(deftest mean-test
  (testing "mean"
    (is (= 5 (mean [2 4 4 4 5 5 7 9])))))

(deftest modes-test
  (testing "modes"
    (is (= [1]   (modes [1 2 3 4 1 5 6 7 1 2])))
    (is (= [1 2] (modes [1 2 3 4 1 5 6 2 1 2])))))

(deftest variance-test
  (testing "variance"
    (is (= 4 (variance [2 4 4 4 5 5 7 9])))))

(deftest std-dev-test
  (testing "std-dev"
    (is (= 2 (std-dev [2 4 4 4 5 5 7 9])))))

(deftest catalan-test
  (testing "catalan"
    (is (= '(1 1 2 5 14 42 132 429 1430 4862 16796 58786 208012 742900 2674440)
           (take 15 (catalan))))))

(deftest geometric-mean-test
  (testing "geometric-mean"
    (is (= 4.603215596046737 (geometric-mean [2 4 4 4 5 5 7 9])))))

(deftest harmonic-mean-test
  (testing "harmonic-mean"
    (is (= 10080/2399 (harmonic-mean [2 4 4 4 5 5 7 9])))))

(deftest root-mean-square-test
  (testing "root-mean-square"
    (is (= 6.2048368229954285 (root-mean-square (range 1 11))))))

(deftest karatsuba-test
  (testing "karatsuba"
    (is (=        0  (karatsuba     0  1234)))
    (is (=       12  (karatsuba     3     4)))
    (is (=  7006652  (karatsuba  5678  1234)))
    (is (= -7006652  (karatsuba -5678  1234)))
    (is (= -7006652  (karatsuba  5678 -1234)))
    (is (=  7006652  (karatsuba -5678 -1234)))
    (is (= 2398052144256972738312111343740159860488704
           (karatsuba 1548564532135665645648 1548564554135665641248)))
    (is (= 1881676371789154860897069
           (karatsuba 123456789 123456789 123456789)))))

(deftest prime-sieve-test
  (testing "prime-sieve"
    (is (= [2 3 5 7 11 13 17 19 23 29] (take 10 (prime-sieve))))
    (is (= 104743 (nth (prime-sieve) 10000)))))

(deftest weighted-random-sample-test
  (testing "weighted-random-sample"
    (let [coin {:heads 0.75 :tails 0.25}
          tosses (frequencies
                   (for [x (range 1000000)]
                     (weighted-random-sample coin)))
          head-count (:heads tosses)
          tail-count (:tails tosses)]
      (is (> 0.01 (float (std-dev [3 (/ head-count tail-count)])))))))

(deftest hash-join-test
  (testing "hash-join"
    (let [rgb '({:color "red"   :hex "ff0000"}
                {:color "green" :hex "00ff00"}
                {:color "blue"  :hex "0000ff"})
          hsl '({:color "red"   :val [  0 100 50]}
                {:color "green" :val [120 100 50]}
                {:color "blue"  :val [240 100 50]})]
      (is (=  '({:color "red"   :hex "ff0000" :val [  0 100 50]}
                {:color "green" :hex "00ff00" :val [120 100 50]}
                {:color "blue"  :hex "0000ff" :val [240 100 50]})
              (hash-join rgb :color hsl :color))))))

(deftest vec-to-tree-test
  (testing "vec-to-tree"
    (is (= {:val 1 :left
            {:val 2 :left
             {:val 4 :left
              {:val 7 :left nil
               :right nil}
              :right nil}
             :right
             {:val 5 :left nil
              :right nil}}
            :right
            {:val 3 :left
             {:val 6 :left
              {:val 8 :left nil
               :right nil}
              :right
              {:val 9 :left nil
               :right nil}}
             :right nil}}
          (vec-to-tree [1 [2 [4 [7]] [5]] [3 [6 [8] [9]]]])))
    (is (= :not-a-tree (vec-to-tree :not-a-tree)))))

(deftest fibonacci-test
  (testing "fibonacci"
    (is (= [0 1 1 2 3 5 8 13 21 34] (take 10 fibonacci)))))
