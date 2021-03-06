(ns clojure-solutions.util
  (:require [clojure.string :as str]))

;;; Parsing

(defn str-to-coll-base
  "Convert a string into a collection, reading every character in the
  given base; e.g, \"12312094\" -> '(1 2 3 1 2 0 9 4) in base 10."
  [b s]
  (map #(Character/digit ^char % ^int b) s))

(defn coll-to-base
  "Convert a collection a number in the given base; e.g.,
  '(1 2 3 1 2 0 9 4) -> `12312094' in base 10."
  [b xs]
  (BigInteger. ^String (apply str xs) ^int b))

(defn words [s]
  (str/split s #" "))

(defn split-groups [s]
  (str/split s #"\n\n"))

;;; Pretty printing

(defn plot-set
  "Plot a set of points of the form [x y]; ASCII style."
  [s]
  (let [[xmax ymax] (reduce (fn [[x-max y-max] [x y]]
                              [(max x-max x) (max y-max y)])
                            s)
        [xmin ymin] (reduce (fn [[x-min y-min] [x y]]
                              [(min x-min x) (min y-min y)])
                            s)]
    (map (fn [line] (str/join
                     (map (fn [[a b]] (if (s [a b]) "█" " "))
                          line)))
         (map (fn [y] (map #(vector % y)
                           (range xmin (inc xmax))))
              (range ymin (inc ymax))))))

;;; Stuff that should be in clojure.core

(defn sum [xs]
  (reduce + xs))

(defn transpose [mat]
  (apply mapv vector mat))

(defn elem [x xs]
  (some #{x} xs))
;;Execution time mean : 486.917037 ns

;;faster path using identity, since we're comparing keywords.
#_
(defn elem-id [x xs]
  (some #(identical? % x) xs))
;;Execution time mean : 183.413372 ns

;;avoid seq coercion and leverage internal reduction
;;where possible with vectors.  no first/rest looping.
(defn elem-id [x xs]
  (reduce (fn [acc v]
            (if (identical? x v)
              (reduced v)
              acc)) nil xs))
;;Execution time mean : 13.682727 ns

;;use eduction without apply and length fns...
(defmacro educt [& xforms]
  `(clojure.core.Eduction. (comp ~@(butlast xforms)) ~(last xforms)))

;;eliminate comp.
(defmacro inline-comp [& xforms]
  (let [binds (for [f xforms]
                [(gensym "func") f])
        fs   (reverse (map first binds))]
    `(let [~@(reduce (fn [acc [f x]] (conj acc f x)) [] binds)]
       (fn [x#] (-> x# ~@fs)))))

;;(let [f (u/inline-comp #(+ 3 %) #(* % 2) #(- % 1))] (c/quick-bench (f 10)))
;;Evaluation count : 16478064 in 6 samples of 2746344 calls.
;;Execution time mean : 34.611968 ns

;;(let [f (comp #(+ 3 %) #(* % 2) #(- % 1))] (c/quick-bench (f 10)))
;;Evaluation count : 10046652 in 6 samples of 1674442 calls.
;;Execution time mean : 59.544141 ns

;;now using inline-comp
(defmacro faster-educt [& xforms]
  `(clojure.core.Eduction. (inline-comp ~@(butlast xforms)) ~(last xforms)))

;;a bit faster than set coercion.
(defn distinct! ^java.util.HashSet [xs]
  (->> xs
       (reduce (fn [^java.util.HashSet acc x]
                 (doto acc (.add x)))
               (java.util.HashSet.))))


(defn memo-1 [f]
  (let [cache (java.util.HashMap.)]
    (fn [x]
      (let [known (.get cache x)]
        (if (nil? known)
          (let [res (f x)
                _   (.put cache x res)]
            res)
          known)))))

;;count an eduction.
(defn ecount [xf coll]
  (transduce xf (completing (fn [acc _] (unchecked-inc acc))) 0 coll))

;;alternate variant using computeIfAbsent and
;;java.util.function.Function blech.
;;this is actually a bit slower since it
;;adds a null check for safety...

;;lame wrapper.
(comment
  (defn jf [f]
    (reify java.util.function.Function
      (apply [this k]
        (f k))))

  (defn memo-1 [f]
    (let [cache (java.util.HashMap.)
          f     (jf f)]
      (fn [x]
        (.computeIfAbsent cache x f))))
)

(defn permutations [[h & t :as coll]]
  (if (nil? t)
    [coll]
    (for [head coll
          tail (permutations (disj (set coll) head))]
      (cons head tail))))

(defn converge
  "Apply a function `f' until it converges."
  [f xs]
  (let [xss (f xs)]
    (if (= xss xs) xs (recur f xss))))

(defn converge-when
  "Apply a function `f' until it converges and count how long that takes."
  ([f xs]   (converge-when f xs 0))
  ([f xs n]
   (let [xss (f xs), m (inc n)]
     (if (= xss xs) [m xs] (recur f xss m)))))

(defn map-val
  "Map over the values of a given map."
  [f hmap]
  (into {} (map (fn [[k v]] {k (f v)})) hmap))

(defn filter-val
  "Filter a map by applying `f' to its values."
  ([f]      (filter #(f (val %))))
  ([f hmap] (filter #(f (val %)) hmap)))

(defn map-from-coll-with
  "Turn a collection into a map.  First map `g' over every element to
  produce a map and then merge the resulting maps, applying `f' in case
  of duplicate keys.  For example:

    (map-from-coll-with + #(hash-map % 1) [1 2 3 1]) ≡ {1 2, 3 1, 2 1}."
  [f g coll]
  (apply merge-with f (map g coll)))

(defn map-from-coll
  "Like `map-from-coll-with', but ignore the second value in case of a
  conflict."
  [g coll]
  (map-from-coll-with (fn [a _] a) g coll))

;;; Matrix manipulation

(defn mat-ix
  "Return the element at index (i, j).  Returns `nil' if index is
  out-of-bounds."
  [m [i j]]
  (let [rows (count m)
        cols (count (first m))]
    (when (and (< -1 i rows) (< -1 j cols))
      (nth (nth m i) j))))

(defn map-matrix
  "Map a function f(i, j, el) over all elements of a matrix with
  indices."
  [f mat]
  (apply concat
         (keep-indexed (fn [i row]
                         (keep-indexed (fn [j el]
                                         (f i j el))
                                       row))
                       mat)))
