(ns hypercrud.client.util)


(defn map-values [f m]
  (->> (map (fn [[k v]] [k (f v)]) m)
       (into {})))
