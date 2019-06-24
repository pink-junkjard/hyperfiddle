(ns contrib.datomic.common.query
  (:require
    [clojure.spec.alpha :as s]
    [contrib.datomic.client.query]))


(defn q->map [q]
  ; todo common spec for validation
  #_{:pre [(not (s/explain-data :contrib.datomic.client/query q))]
     :post [(not (s/explain-data :contrib.datomic.client.query/query-map %))]}
  (if (map? q)
    q
    (let [xform (comp
                  (partition-by keyword?)
                  (partition-all 2)
                  (map (fn [[[kw] v]] [kw (vec v)])))]
      (into {} xform q))))

(defn q->list [q]
  ; todo common spec for validation
  #_{:pre [(not (s/explain-data :contrib.datomic.client/query q))]
     :post [(not (s/explain-data :contrib.datomic.client.query/query-list %))]}
  (if (sequential? q)
    q
    #_(let [xform (comp (map (fn [[kw v]] (cons kw (vec v)))) cat)] ; will break on client spec due to ordering issues
        (into [] xform q))
    (cond-> (vec (cons :find (:find q)))
      (:keys q) (into (cons :keys (:keys q)))
      (:syms q) (into (cons :syms (:syms q)))
      (:strs q) (into (cons :strs (:strs q)))
      (:with q) (into (cons :with (:with q)))
      (:in q) (into (cons :in (:in q)))
      (:where q) (into (cons :where (:where q))))))

(defn append-inputs [q & inputs]
  ; todo common spec for validation
  #_{:pre [(s/valid? (s/+ :contrib.datomic.client.query/input) inputs)]}
  (if (sequential? q)
    (-> (apply append-inputs (q->map q) inputs) q->list)
    (update q :in concat inputs)))

(defn append-where-clauses [q & where-clauses]
  ; todo common spec for validation
  #_{:pre [(s/valid? (s/+ :contrib.datomic.client.query/clause) where-clauses)]}
  (if (sequential? q)
    (-> (apply append-where-clauses (q->map q) where-clauses) q->list)
    (update q :where concat where-clauses)))
