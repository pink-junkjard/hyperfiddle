(ns hypercrud.client.util)


(defn map-values [f m]
  (->> (map (fn [[k v]] [k (f v)]) m)
       (into {})))


(defn match [values matches & default]
  (let [result (->> matches
                    (filter (fn [match]
                              (let [[tail & rest] (reverse match)]
                                (->> (vec (reverse rest))
                                     (map vector values)
                                     (every? (fn [[actual potential-match-value]]
                                               (= actual potential-match-value)))))))
                    first
                    last)]
    (if result
      (result)
      (if default                                           ;bug somewhere in here
        (default)
        (throw "failed to match")))))
