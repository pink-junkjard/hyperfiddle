(ns contrib.datomic-tx
  (:require
    [clojure.set :as set]
    [contrib.datomic :refer [tempid?]]))


(defn edit-entity "o/n are sets in the :many case"
  [id attribute o n]
  (let [{a :db/ident {cardinality :db/ident} :db/cardinality} attribute]
    (case cardinality
      :db.cardinality/one
      (cond-> []
        (some? o) (conj [:db/retract id a o])
        (some? n) (conj [:db/add id a n]))

      :db.cardinality/many
      (let [o (set o)
            n (set n)]
        (vec (concat (map (fn [v] [:db/retract id a v]) (set/difference o n))
                     (map (fn [v] [:db/add id a v]) (set/difference n o))))))))

(defn- simplify-oeav [simplified-tx next-stmt]
  (let [[op e a v] next-stmt
        g (group-by (fn [[op' e' a' v']] (and (= e' e) (= a' a) (= v' v)))
                    simplified-tx)
        [op' e' a' v'] (first (get g true))                 ;if this count > 1, we have duplicate stmts, they are harmless and discard dups here.
        unrelated (get g false)]
    (case op
      :db/add (if (= op' :db/retract)
                unrelated                                   ;we have a related previous stmt that cancels us and it out
                (conj unrelated next-stmt))
      :db/retract (if (= op' :db/add)
                    unrelated                               ;we have a related previous stmt that cancels us and it out
                    (conj unrelated next-stmt)))))

(defn- retract-entity [schema tx-data e]
  (let [{:keys [tx child-entids orphaned-parent-check]}
        (reduce (fn [acc next-stmt]
                  (if (contains? #{:db/add :db/retract} (first next-stmt))
                    (let [[op' e' a' v'] next-stmt]
                      (cond
                        (= e e') (if (and (:db/isComponent (get schema a')) (tempid? v'))
                                   (update acc :child-entids conj v')
                                   acc)
                        (= e v') (if (tempid? e')
                                   (update acc :orphaned-parent-check conj e')
                                   acc)
                        :else (update acc :tx conj next-stmt)))
                    (update acc :tx conj next-stmt)))
                {:tx []
                 :child-entids []
                 :orphaned-parent-check []}
                tx-data)
        tx (loop [[entid & rest] orphaned-parent-check
                  tx tx]
             (if entid
               (if (some #(and (= :db/add (first %)) (= entid (second %))) tx)
                 (recur rest tx)                            ; entid used in entity position in statements
                 (let [{:keys [tx orphaned-parent-check]}
                       (reduce (fn [acc next-stmt]
                                 (if (and (= :db/add (first next-stmt)) (= entid (last next-stmt)))
                                   (update acc :orphaned-parent-check conj (second next-stmt))
                                   (update acc :tx conj next-stmt)))
                               {:tx []
                                :orphaned-parent-check []}
                               tx)]
                   (recur (concat rest orphaned-parent-check) tx)))
               tx))]
    (loop [[entid & rest] child-entids
           tx tx]
      (if entid
        (recur rest (retract-entity schema tx entid))
        tx))))

(defn- simplify [schema simplified-tx next-stmt]
  (condp contains? (first next-stmt)
    #{:db/add :db/retract} (simplify-oeav simplified-tx next-stmt)
    #{:db/retractEntity :db.fn/retractEntity} (let [e (second next-stmt)]
                                                (cond-> (retract-entity schema simplified-tx e)
                                                  (not (tempid? e)) (conj next-stmt)))
    (conj simplified-tx next-stmt)))

(defn into-tx [schema tx more-statements]
  "We don't care about the cardinality (schema) because the UI code is always
  retracting values before adding new value, even in cardinality one case. This is a very
  convenient feature and makes the local datoms cancel out properly always to not cause
  us to re-assert datoms needlessly in datomic"
  (reduce (partial simplify schema) tx more-statements))

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
