(ns hypercrud.util)


(defn map-values [f m]
  (->> (map (fn [[k v]] [k (f v)]) m)
       (into {})))


(defn group-by-assume-unique [f xs]
  (->> xs
       (map (juxt f identity))
       (into {})))


(defn update-existing [m k f]
  (if (get m k)
    (update m k f)
    m))
