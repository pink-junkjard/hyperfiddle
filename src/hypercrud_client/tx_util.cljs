(ns hypercrud-client.tx-util
  (:require [cljs.core.match :refer-macros [match]]))


(defn tempid? [eid] (< eid 0))


(defn entity->datoms [eid data]
  (->> data
       (mapcat (fn [[attr val]]
                 (cond
                   (coll? val) (map (fn [v] [:db/add eid attr v]) val)
                   :else [[:db/add eid attr val]])))
       (into [])))


(defn simplify [simplified-datoms next-datom]
  (let [[op e a v] next-datom
        g (group-by (fn [[op' e' a' v']] (and (= e' e) (= a' a) (= v' v)))
                    simplified-datoms)
        [op' e' a' v'] (first (get g true))                 ;if this count > 1, we have duplicate datoms, they are harmless and discard dups here.
        non-related (get g false)]
    (match [op]
           [:db/add] (if (= op' :db/retract)
                       non-related                          ;we have a related previous datom that cancels us and it out
                       (conj non-related next-datom))

           [:db/retract] (if (= op' :db/add)
                           non-related                      ;we have a related previous datom that cancels us and it out
                           (conj non-related next-datom)))))


(defn normalize-tx [old-datoms new-datoms]
  "We don't care about the cardinality (schema) because the UI code is always
  retracting values before adding new value, even in cardinality one case. This is a very
  convenient feature and makes the local datoms cancel out properly always to not cause
  us to re-assert datoms needlessly in datomic"
  (.log js/console (pr-str new-datoms))
  (reduce simplify old-datoms new-datoms))
