(ns contrib.data
  #?(:cljs (:require-macros [contrib.data]))
  (:require [cats.monad.either :as either]))


(defn map-values [f m]
  (->> (map (juxt key (comp f val)) m)
       (into {})))

(defn map-keys [f m]
  (->> (map (juxt (comp f key) val) m)
       (into {})))

(defn group-by-assume-unique [f xs]
  (->> xs
       (map (juxt f identity))
       (into {})))

(defn group-by-unique [f xs]
  (->> xs
       (map (juxt f identity))
       (reduce (fn [acc [k _ :as kv]]
                 (if (contains? acc k)
                   (throw (ex-info "Duplicate key" k))
                   (conj acc kv)))
               {})))

(defn update-existing [m k f & args]
  (if (get m k)
    (apply update m k f args)
    m))

(defn take-to
  "Returns a lazy sequence of successive items from coll while (pred item) returns true.
  Unlike take-while, it includes the last item."
  [pred coll]
  (lazy-seq
    (when-let [s (seq coll)]
      (let [me (first s)]
        (cons me (when (pred me) (take-to pred (rest s))))))))

(defn parse-query-element [q query-element]
  ; return value is symbols, not strings
  (let [last-kw (atom nil)
        f (fn [x]
            (if (keyword? x) (reset! last-kw x))
            @last-kw)]
    (->> (partition-by f q)
         (filter #(= query-element (first %)))
         first
         (drop 1))))

(defn transpose "Define transpose empty matrix to return the matrix unchanged - this is not math"
  [matrix]
  (if (seq matrix) (apply mapv vector matrix)
                   matrix))

(defn zip [as bs]
  ;(transpose [as bs])
  (map vector as bs))

(defn pad [n zero coll]
  (take n (concat coll (repeat zero))))

(defn map-pad [zero]
  (fn [f & cols]
    (let [n (apply max (map count cols))
          cols (map #(pad n zero %) cols)]
      (apply map f cols))))

;(defn fallback [p v not-found]
;  (if-not (p v) v not-found))

(defn fvor [f default]
  (fn [v & args]
    (or (if v (apply f v args)) default)))

(defn tee [g f!] (fn [v] (f! v) (g v)))

(defn kwargs
  "arg format is kwargs first; trailing non-kw args are nil key
      [:a 1 :b 2 'a 'b 'c] => {nil (a b c), :b 2, :a 1}"
  [as]
  (let [[kwargs args] (split-with (comp keyword? first) (partition-all 2 as))
        args (flatten args)]
    (-> (apply hash-map (flatten kwargs))
        (as-> $ (if (seq args) (assoc $ nil args) $)))))

(defn abs-normalized [x]
  #?(:clj  (if x (Math/abs x) 0)
     :cljs (js/Math.abs x)))

(defn unwrap [v']
  ; On the api side, we never inspect the error, the either is useless
  (either/branch v' (constantly nil) identity))

(defn xorxs [xorxs]
  (cond (vector? xorxs) xorxs
        (seq? xorxs) xorxs
        (nil? xorxs) nil
        :else-single-value [xorxs] #_"can be a map"))

(defn group-by-pred [f? xs]
  (let [{a true b false} (group-by f? xs)]
    [a b]))

(defn filter-keys [f? m]
  (->> m (filter (fn [[k v]] (f? k))) (into {})))

(defn orp "or with custom predicate" [f? & args]            ; todo macro
  (first (remove f? args)))

(defmacro cond-let [& clauses]
  (when clauses
    (list 'if-let (first clauses)
          (second clauses)
          (cons `cond-let (next (next clauses))))))

(defn rtrim-coll [f? xs]
  (->> (reverse xs)
       (drop-while f?)
       (reverse)
       (into (empty xs))))

(defn fix-arity [f n]
  (if f
    (fn [& args]
      (apply f (take n args)))))

(defn compare-by-index [ordering]
  (let [index (into {} (map vector ordering (range)))]
    #(compare (index %1) (index %2))))

(defn ancestry-common [as bs]
  (->> (take-while (partial apply =) (zip as bs))
       (map first)))

(defn ancestry-divergence [as bs]
  (->> (drop-while (partial apply =) (zip as bs))
       (map first)))

(comment
  (ancestry-divergence [:body 0 :reg/gender] [:body 0 :reg/shirt-size])
  )