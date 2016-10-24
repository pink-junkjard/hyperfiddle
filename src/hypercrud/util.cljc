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
