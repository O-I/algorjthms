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
(defn sqr
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
  (+ (math/abs ^Integer (- x2 x1)) (math/abs ^Integer (- y2 y1))))

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

(defn extended-gcd
  "Extended Euclidean algorithm"
  [a b]
  (if (zero? b) [1 0]
    (let [[q r] [(quot a b) (rem a b)]
          [s t] (extended-gcd b r)]
      [t (- s (* q t))])))

(defn modular-inverse
  "Calculates x such that a * x = 1 mod m.
   If a and m are not coprime, raises an error."
   [a m]
   (let [[x y] (extended-gcd a m)]
     (if (= y 1)
       (mod x m)
       (throw (Exception. (str "=> " a " and " m " are not coprime."))))))

(defn roots
  "Finds the roots of a function given a range, step, and tolerance."
  [f start stop step eps]
  (filter #(-> (f %) math/abs (< eps)) (range start stop step)))

(defn swap
  "Swaps two elements at positions i and j in vector v"
  [v i j]
  (assoc v j (v i) i (v j)))

(defn knuth-shuffle [vect]
  (reduce (fn [v i] (swap v (rand-int i) i))
          vect (range (-> vect count dec) 1 -1)))

(defn neighbors
  "Returns all adjacent elements to a given index in a 2D matrix."
  ([size yx] (neighbors [[-1 0] [1 0] [0 -1] [0 1]] size yx))
  ([deltas size yx]
    (filter (fn [new-yx]
              (every? #(< -1 % size) new-yx))
            (map #(vec (map + yx %)) deltas))))

(defn estimate-cost
  "A straight-line h function to estimate remaining path cost."
  [step-cost-est size y x]
  (* step-cost-est
     (- (+ size size) y x 2)))

(defn path-cost
  "A g function that calculates the cost of a path so far."
  [node-cost cheapest-nbr]
  (+ node-cost
     (:cost cheapest-nbr 0)))

(defn total-cost
  "An f function that sums the cost of a path so far g
   with the estimated remaining cost of that path h."
  [new-cost step-cost-est size y x]
  (+ new-cost
     (estimate-cost step-cost-est size y x)))

(defn min-by [f coll]
  (when (seq coll)
    (reduce (fn [min this]
              (if (> (f min) (f this)) this min))
             coll)))

; from The Joy of Clojure
(defn A* [start-yx step-est cell-costs]
  (let [size (count cell-costs)]
    (loop [steps 0
           routes (vec (replicate size (vec (replicate size nil))))
           work-todo (sorted-set [0 start-yx])]
      (if (empty? work-todo)
        [(peek (peek routes)) :steps steps]
        (let [[_ yx :as work-item] (first work-todo)
               rest-work-todo (disj work-todo work-item)
               nbr-yxs (neighbors size yx)
               cheapest-nbr (min-by :cost
                                    (keep #(get-in routes %)
                                          nbr-yxs))
                      new-cost (path-cost (get-in cell-costs yx)
                                         cheapest-nbr)
                      old-cost (:cost (get-in routes yx))]
          (if (and old-cost (>= new-cost old-cost))
             (recur (inc steps) routes rest-work-todo)
             (recur (inc steps)
                    (assoc-in routes yx
                              {:cost new-cost
                               :yxs (conj (:yxs cheapest-nbr [])
                                   yx)})
             (into rest-work-todo
                   (map
                     (fn [w]
                       (let [[y x] w]
                         [(total-cost new-cost step-est size y x) w]))
                     nbr-yxs)))))))))

(defn index
  "Generates a uniform representation of an indexed collection."
  [coll]
  (cond
    (map? coll) (seq coll)
    (set? coll) (map vector coll coll)
    :else (map vector (iterate inc 0) coll)))

; from the Joy of Clojure
(defn position
  "Returns the positions of all elements in coll that satisfy pred."
  [pred coll]
  (for [[i v] (index coll) :when (pred v)] i))

(defn horner
  "Given a polynomial represented as a list of its coefficients in
  ascending order, computes its value at a point using Horner's rule."
  [coeffs x]
  (reduce #(-> %1 (* x) (+ %2)) (reverse coeffs)))

; from Rosetta Code
; http://rosettacode.org/wiki/AKS_test_for_primes#Clojure
(defn- coeff
  "kth coefficient of (x - 1)^n"
  [n k]
  (/ (apply *' (range n (- n k) -1))
     (apply *' (range k 0 -1))
     (if (and (even? k) (< k n)) -1 1)))

(defn- coeffs
  "coefficient series for (x - 1)^n, k=[0..n]"
  [n]
  (map #(coeff n %) (range (inc n))))

(defn aks? [p] (->> (coeffs p) rest butlast (every? #(-> % (mod p) zero?))))

(defn digit-count
  "Returns the number of digits in an integer."
  [z]
  (if (zero? z)
      1
      (count (take-while pos? (iterate #(quot % 10) (math/abs z))))))

(defn choose
  "Calculates the binomial coefficient n choose k"
  [n k]
  (let [rprod (fn [a b] (reduce * (range a (inc b))))]
    (/ (rprod (- n k -1) n) (rprod 1 k))))

(defn- make-dict []
  (let [vals (range 0 256)]
    (zipmap (map (comp #'list #'char) vals) vals)))

(defn lempel-ziv-welch
  "Performs lossless data compression of a string."
  [#^String text]
  (loop [t (seq text)
         r '()
         w '()
         dict (make-dict)
         s 256]
    (let [c (first t)]
      (if c
        (let [wc (cons c w)]
          (if (get dict wc)
            (recur (rest t) r wc dict s)
            (recur (rest t) (cons (get dict w) r) (list c) (assoc dict wc s) (inc s))))
        (reverse (if w (cons (get dict w) r) r))))))

(defn mean [samples]
  (/ (reduce + samples) (count samples)))

(defn modes [coll]
  (let [dist (frequencies coll)
        [value freq] [first second]
        sorted (sort-by (comp - freq) dist)
        maxfq (freq (first sorted))]
    (map value (take-while #(= maxfq (freq %)) sorted))))

(defn variance [samples]
  (let [n (count samples)
        avg (mean samples)
        intermediate (map #(math/expt (- %1 avg) 2) samples)]
    (mean intermediate)))

(defn std-dev [samples]
  (-> samples variance math/sqrt))

(defn factorial [n]
  (reduce *' (range 1 (inc n))))

(def ! (memoize factorial))

(defn catalan []
  (map #(/ (! (*' 2 %))
           (*' (! (inc %)) (! %))) (range)))

(defn geometric-mean [samples]
  (math/expt (reduce * samples) (-> samples count /)))

(defn harmonic-mean [samples]
  (-> (->> samples (map /)) mean /))

(defn root-mean-square [samples]
  (math/sqrt (/ (reduce + (map sqr samples)) (count samples))))

(defn- k* [m n]
  (let [sign (if (= (neg? m) (neg? n)) + -)
        m (math/abs m) n (math/abs n)]
    (if (or (< m 10) (< n 10))
          (* m n)
          (let [e  (quot (max (digit-count m) (digit-count n)) 2)
                a  (quot m (math/expt 10 e))
                b  (rem  m (math/expt 10 e))
                c  (quot n (math/expt 10 e))
                d  (rem  n (math/expt 10 e))
                z0 (k* b d)
                z1 (k* (+ a b) (+ c d))
                z2 (k* a c)]
            (sign (+ (*' z2 (math/expt 10 (* 2 e)))
                     (*' (- z1 z2 z0) (math/expt 10 e))
                     z0))))))

(defn karatsuba [& multiplicands]
  (reduce k* multiplicands))

; by Christophe Grande
; http://clojure.roboloco.net/?p=100
(defn prime-sieve []
  (letfn [(enqueue [sieve n step]
            (let [m (+ n step)]
              (if (sieve m)
                (recur sieve m step)
                (assoc sieve m step))))
          (next-sieve [sieve n]
            (if-let [step (sieve n)]
              (-> sieve
                  (dissoc n)
                  (enqueue n step))
              (enqueue sieve n (+ n n))))
          (next-primes [sieve n]
            (if (sieve n)
              (recur (next-sieve sieve n) (+ n 2))
              (cons n (lazy-seq (next-primes (next-sieve sieve n) (+ n 2))))))]
    (cons 2 (lazy-seq (next-primes {} 3)))))

; Efraimidis & Spirakis
; http://utopia.duth.gr/%7Epefraimi/research/data/2007EncOfAlg.pdf
(defn weighted-random-sample [freq]
  (key (apply max-key #(math/expt (rand) (/ (val %))) freq)))
