(ns contrib.datomic2                                        ; avoid defprotocol reloading issues
  (:require
    [contrib.datomic]))


(defn reachable-pullpaths "
  txfn can be on scalar and it is harmless to allow this."
  [ctx]
  (let [result-path (:hypercrud.browser/result-path ctx)    ; [:domain/databases 17592186046511 :db/id]
        schema @(:hypercrud.browser/schema ctx)
        root-pullshape @(:hypercrud.browser/root-pull-enclosure ctx)]
    ; Start at the top. Traverse. Are we satisfied?
    ; :one is satisfied; :many is satisfied if we have it in the dependency path
    ; So turn result-path into {:domain/databases 17592186046511} ?
    ; This fails in complex pulls
    (let [one? #(contrib.datomic/cardinality? schema % :db.cardinality/one)
          satisfied? (into #{} (filter keyword? result-path))]
      (contrib.datomic/pull-traverse schema root-pullshape #(or (one? %) (satisfied? %))))))

(defn reachable-attrs [ctx]
  (->> (reachable-pullpaths ctx)
       (map last)
       (remove nil?)
       distinct))
