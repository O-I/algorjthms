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
