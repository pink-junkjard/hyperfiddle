(ns contrib.data
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

(defn fallback [p v not-found]
  (if-not (p v) v not-found))

(defn tee [g f!] (fn [v] (f! v) (g v)))

(defn kwargs
  "arg format is kwargs first; trailing non-kw args are nil key
      [:a 1 :b 2 'a 'b 'c] => {nil (a b c), :b 2, :a 1}"
  [as]
  (let [[kwargs args] (split-with (comp keyword? first) (partition-all 2 as))]
    (-> (apply hash-map (flatten kwargs))
        (assoc nil (flatten args)))))

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
