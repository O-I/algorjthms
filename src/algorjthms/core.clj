(ns algorjthms.core
  (:require [clojure.math.numeric-tower :as math]))

; Slight modification of Jean Niklas L'Orange's implementation
; https://groups.google.com/forum/#!topic/clojure/J-L3BUTaJ3E/discussion
(defn kadane
  "Returns the maximum sum of a contiguous subarray of a vector.
   If the vector consists of all negative numbers, the minimum
   negative number is returned. Returns 0 for an empty vector."
  [[head & tail]]
  (let [pos+ (fn [sum x] (if (neg? sum) x (+ sum x)))
        partial-sums (reductions pos+ (or head 0) tail)]
    (reduce max partial-sums)))

; from The Joy of Clojure
(defn- sort-parts
  "Lazy, tail-recursive, incremental quicksort. Works against
   and creates partitions based on the pivot, defined as 'work'."
  [work]
  (lazy-seq
    (loop [[part & parts] work]
      (if-let [[pivot & xs] (seq part)]
        (let [smaller? #(< % pivot)]
          (recur (list*
                   (filter smaller? xs)
                   pivot
                   (remove smaller? xs)
                   parts)))
        (when-let [[x & parts] parts]
          (cons x (sort-parts parts)))))))

(defn quicksort [xs]
  (sort-parts (list xs)))

; from Rosetta Code
; http://rosettacode.org/wiki/Longest_Common_Subsequence#Clojure
(defn- longest [xs ys]
  (if (> (count xs) (count ys)) xs ys))

(def lcs
  (memoize
   (fn [[x & xs] [y & ys]]
     (cond
      (or (nil? x) (nil? y)) nil
      (= x y) (cons x (lcs xs ys))
      :else (longest (lcs (cons x xs) ys) (lcs xs (cons y ys)))))))

(defn hamming
  "Returns the number of positions in which corresponding
   elements are different in two equal length seqs. If they
   are of unequal length, nil is returned."
   [seq1 seq2]
   (when (= (count seq1) (count seq2))
     (count (remove (fn [[x y]] (= x y)) (partition 2 (interleave seq1 seq2))))))

; from the Leiningen source code
(defn- next-dist-row [s t x pprev prev]
  (let [t-len (count t)
        eq-chars (fn [x y] (= (nth s x) (nth t (dec y))))]
    (reduce (fn [row y]
              (let [min-step
                    (cond->
                     (min (inc (peek row)) ; addition cost
                          (inc (get prev y)) ; deletion cost
                          (cond-> (get prev (dec y)) ; substitution cost
                                  (not (eq-chars x y)) inc))
                     (and (pos? x) (pos? (dec y)) ; check for transposition
                          (eq-chars x (dec y))
                          (eq-chars (dec x) y)
                          (not (eq-chars x y)))
                     (min (inc (get pprev (- y 2)))))] ; transposition cost
                (conj row min-step)))
            [(inc x)]
            (range 1 (inc t-len)))))

(defn damerau-levenshtein
  "Returns the Damerau–Levenshtein distance between two strings."
  [s t]
  (let [s-len (count s)
        t-len (count t)
        first-row (vec (range (inc t-len)))
        matrix (reduce (fn [matrix x]
                         (conj matrix
                               (next-dist-row s t x
                                              (peek (pop matrix))
                                              (peek matrix))))
                       [[] first-row]
                       (range s-len))]
    (peek (peek matrix))))

(defn binary-search
  "Searches a sorted collection between optional lower and upper
   indices (default is entire collection) for a target element.
   Returns the index of the element if found, otherwise returns nil."
  ([haystack needle]
    (binary-search haystack needle 0 (dec (count haystack))))
  ([haystack needle lower upper]
    (if (> lower upper) nil
      (let [middle (quot (+ lower upper) 2) mth (nth haystack middle)]
        (cond
          (> mth needle) (recur haystack needle lower (dec middle))
          (< mth needle) (recur haystack needle (inc middle) upper)
          (= mth needle) middle)))))

; Y combinator
(defn Y [f]
  (#(% %) #(f (fn [& args] (apply (% %) args)))))

; explicit, longform Y combinator implementation for reference
; (defn Y [f]
;   ((fn [x] (x x))
;    (fn [x]
;      (f (fn [& args]
;           (apply (x x) args))))))

(defn gcd
  "Returns the greatest common divisor of two integers."
  [a b]
  (if (zero? b) a (recur b (mod a b))))

(defn- merge* [left right]
  (cond (nil? left)  right
        (nil? right) left
        :else (let [[l & *left]  left
                    [r & *right] right]
               (if (<= l r) (cons l (merge* *left right))
                            (cons r (merge* left *right))))))

(defn merge-sort [L]
  (let [[l & *L] L]
    (if (nil? *L)
      L
      (let [[left right] (split-at (/ (count L) 2) L)]
        (merge* (merge-sort left) (merge-sort right))))))

; from the clojure.math.numeric-tower README example
; https://github.com/clojure/math.numeric-tower#example-usage
(defn- sqr
  "Uses the numeric tower expt to square a number"
  [x]
  (math/expt x 2))

(defn euclidean-squared-distance
  "Computes the Euclidean squared distance between two sequences"
  [a b]
  (reduce + (map (comp sqr -) a b)))

(defn euclidean-distance
  "Computes the Euclidean distance between two sequences"
  [a b]
  (math/sqrt (euclidean-squared-distance a b)))

(defn manhattan-distance
  "Calculates the Manhattan distance between two points"
  [[x1 y1] [x2 y2]]
  (+ (Math/abs ^Integer (- x2 x1)) (Math/abs ^Integer (- y2 y1))))

(defn power-mod
  "Calculates modular exponentiation b^e mod m"
  [b e m]
  (defn m* [p q] (mod (* p q) m))
  (loop [b b e e x 1]
    (if (zero? e) x
      (if (even? e) (recur (m* b b) (/ e 2) x)
        (recur (m* b b) (quot e 2) (m* b x))))))

(defn lcm
  "Returns the least common multiple of two integers."
  [a b]
  (/ (* a b) (gcd a b)))
