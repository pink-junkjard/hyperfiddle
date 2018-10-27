(ns contrib.datomic-tx
  (:require
    [clojure.set :as set]))


(defn edit-entity [id attribute o n]
  (let [{a :db/ident {cardinality :db/ident} :db/cardinality} attribute]
    (case cardinality
      :db.cardinality/one
      (cond-> []
        o (conj [:db/retract id a o])
        n (conj [:db/add id a n]))

      :db.cardinality/many
      (let [o (set o)
            n (set n)]
        (vec (concat (map (fn [v] [:db/retract id a v]) (set/difference o n))
                     (map (fn [v] [:db/add id a v]) (set/difference n o))))))))

(defn simplify [simplified-tx next-stmt]
  (let [[op e a v] next-stmt
        g (group-by (fn [[op' e' a' v']] (and (= e' e) (= a' a) (= v' v)))
                    simplified-tx)
        [op' e' a' v'] (first (get g true))                 ;if this count > 1, we have duplicate stmts, they are harmless and discard dups here.
        non-related (get g false)]
    (case op
      :db/add (if (= op' :db/retract)
                non-related                                 ;we have a related previous stmt that cancels us and it out
                (conj non-related next-stmt))
      :db/retract (if (= op' :db/add)
                    non-related                             ;we have a related previous stmt that cancels us and it out
                    (conj non-related next-stmt))

      ; else probably a :db.fn
      (conj non-related next-stmt))))

(defn into-tx [tx more-statements]
  "We don't care about the cardinality (schema) because the UI code is always
  retracting values before adding new value, even in cardinality one case. This is a very
  convenient feature and makes the local datoms cancel out properly always to not cause
  us to re-assert datoms needlessly in datomic"
  (reduce simplify tx more-statements))

(letfn [(update-v [id->tempid schema a v]
          (if (= :db.type/ref (get-in schema [a :db/valueType :db/ident]))
            (get id->tempid v v)
            v))
        (add-ret [id->tempid schema [op e a v]]
          (let [e (get id->tempid e e)
                v (update-v id->tempid schema a v)]
            [op e a v]))
        (retractEntity [id->tempid schema [op e]]
          [op (get id->tempid e e)])
        (cas [id->tempid schema [op e a ov nv]]
          [op
           (get id->tempid e e)
           a
           (update-v id->tempid schema a ov)
           (update-v id->tempid schema a nv)])]
  (defn stmt-id->tempid "Deep introspection of args to transaction fns in order to reverse tempids"
    [id->tempid schema [op :as stmt]]
    (let [f (case op
              :db/add add-ret
              :db/retract add-ret
              :db/retractEntity retractEntity
              :db.fn/retractEntity retractEntity
              :db/cas cas
              :db.fn/cas cas)]
      (f id->tempid schema stmt))))

(defn ^:export find-datom "not a good abstraction" [tx e-needle a-needle]
  (let [[[_ _ _ v]] (->> tx (filter (fn [[op e a v]]
                                      (= [op e a] [:db/add e-needle a-needle]))))]
    v))
