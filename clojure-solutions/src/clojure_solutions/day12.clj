(ns clojure-solutions.day12
  (:require [clojure.string :as str]
            [clojure.set    :as set]
            [clojure-solutions.util :as u])
  #_(:use [clojure-solutions.util] :reload))

(defn- parse []
  (->> (slurp "./input/day12.txt")
       str/split-lines
       (map #(str/split % #"-"))
       (map (fn [[k v]]                 ; paths are bidirectional
              (into {}
                    (u/filter-val #(not (u/elem-id :start %)))
                    {(keyword k) #{(keyword v)}
                     (keyword v) #{(keyword k)}})))
       (apply merge-with set/union)))
#_
(defn small-cave? [kw]
  (= (name kw) (str/lower-case (name kw))))

(def small-cave? (u/memo-1 (fn [kw] (= (name kw) (str/lower-case (name kw))))))

#_
(defn- small-cave-twice [path]
  (let [small-path (filter small-cave? path)]
    (not= (count small-path) (count (set small-path)))))

;;filter into a vector, count is now o(1), traversal is faster.
#_
(defn- small-cave-twice [path]
  (let [small-path (filterv small-cave? path)]
    (not= (count small-path) (count (distinct! small-path)))))

;;bypassing clojure.core/count genericity (75 ns) for method invoke (4ns)
#_
(defn- small-cave-twice [path]
  (let [small-path (filterv small-cave? path)]
    (not= (count small-path) (.size (u/distinct! small-path)))))

;;bypassing clojure.core/count
(defn- small-cave-twice [path]
  (let [small-path (filterv small-cave? path)]
    (not= (.count ^clojure.lang.Counted small-path)
          (.size (u/distinct! small-path)))))

#_
(defn- solve [keep connections]
  (letfn ((go [path kw]
              (let [path* (conj path kw) ; new path
                    ;; Places we can visit
                    downs (filter #(keep path* %) (connections kw))
                    ]
                (if (= kw :end)
                  [path*]
                  (mapcat #(go path* %) downs)))))
    (->> (connections :start)
         (mapcat #(go [:start] %))
         count)))

;;use nested eductions to avoid realizing sequences, akin to haskell's
;;strictness analysis.
#_
(defn- solve [keep connections]
  (letfn ((go [path kw]
              (let [path* (conj path kw) ; new path
                    ;; Places we can visit
                    downs (eduction (filter #(keep path* %)) (connections kw))]
                (if (= kw :end)
                  [path*]
                  (eduction (mapcat #(go path* %)) downs)))))
    (->> (connections :start)
         (into [] (mapcat #(go [:start] %)))
         count)))

;;avoid the jank in clojure.core/eduction around sequences and apply.
;;751 ms.
#_
(defn- solve [keep connections]
  (letfn ((go [path kw]
              (let [path* (conj path kw) ; new path
                    ;; Places we can visit
                    downs (u/educt (filter #(keep path* %)) (connections kw))]
                (if (= kw :end)
                  [path*]
                  (u/educt (mapcat #(go path* %)) downs)))))
    (->> (connections :start)
         (into [] (mapcat #(go [:start] %)))
         count)))

;;no intermediate vector construction.
#_
(defn- solve [keep connections]
  (letfn ((go [path kw]
              (let [path* (conj path kw) ; new path
                    ;; Places we can visit
                    downs (u/educt (filter #(keep path* %)) (connections kw))]
                (if (= kw :end)
                  [path*]
                  (u/educt (mapcat #(go path* %)) downs)))))
    (->> (connections :start)
         (transduce (mapcat #(go [:start] %)) (completing (fn [acc _]
                                                            (unchecked-inc acc))) 0))))

;;avoid clojure.lang.Util.equiv for identical?
(defn- solve [keep connections]
  (letfn ((go [path kw]
              (let [path* (conj path kw) ; new path
                    ;; Places we can visit
                    downs (u/educt (filter #(keep path* %)) (connections kw))]
                (if (identical? kw :end)
                  [path*]
                  (u/educt (mapcat #(go path* %)) downs)))))
    (->> (connections :start)
         (transduce (mapcat #(go [:start] %))
              (completing (fn [acc _]
                            (unchecked-inc acc))) 0))))

;; => 4754
(defn- part1 [xs]
  (solve (fn [path x] (not (and (small-cave? x) (u/elem-id x path))))
         xs))

#_
(defn- part2 [xs]
  (solve (fn [path x]
           ;; If a small cave already appears twice, make sure it
           ;; doesn't happen again.  Otherwise, just let everything
           ;; through.
           (not (and (small-cave-twice path)
                     (small-cave? x)
                     (u/elem-id x path))))
         xs))

;; => 143562
;;haskell implementation wisely ordered the conditions to prune easier with
;;less effort :), we copy.
(defn- part2 [xs]
  (solve (fn [path x]
           ;; If a small cave already appears twice, make sure it
           ;; doesn't happen again.  Otherwise, just let everything
           ;; through.
           (not (and (small-cave? x) ;;order of conditions from least -> most like haskell's
                     (u/elem-id x path)
                     (small-cave-twice path))))
         xs))

(defn day12 []
  (let [connections (parse)]
    (println (part1 connections))
    (println (part2 connections))))
