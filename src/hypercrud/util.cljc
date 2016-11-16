(ns hypercrud.util)


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


(defn update-existing [m k f]
  (if (get m k)
    (update m k f)
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
