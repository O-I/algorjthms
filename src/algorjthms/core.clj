(ns algorjthms.core)

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
