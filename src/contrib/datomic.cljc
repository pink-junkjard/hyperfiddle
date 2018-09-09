(ns contrib.datomic
  (:require
    [clojure.walk :refer [prewalk]]
    #?(:clj [datomic.api :as d])
    [loom.alg-generic :as loom]))


(defn smart-identity [v]
  (or (:db/ident v) (:db/id v) v))

#?(:clj
   (defn ref? [$ k]
     (= :db.type/ref (->> k (d/entity $) :db/valueType))))

#?(:clj
   (defn one? [$ k]
     (= :db.cardinality/one (->> k (d/entity $) :db/cardinality))))

#?(:clj
   (defn many? [$ k]
     (= :db.cardinality/many (->> k (d/entity $) :db/cardinality))))

#?(:clj
   (defn component? [$ k]
     (->> k (d/entity $) :db/isComponent)))

#?(:clj
   (defn datomic-entity-successors
     "Excludes component because component is considered part of its parent"
     [$ e]
     (->> (-> (d/pull $ ['*] e)
              (dissoc :db/id))
          (mapcat (fn [[k v]]
                    (if (and (ref? $ k) (not (component? $ k)))
                      (cond (one? $ k) [(:db/id v)]
                            (many? $ k) (mapv :db/id v))))))))

(defn alter-ids [m #_":: id -> tempid" tree]
  (prewalk (fn [tree]
             (if (map? tree)
               (if-let [id (m (:db/id tree))]
                 (assoc tree :db/id id)
                 (dissoc tree :db/id))
               tree))
           tree))

(defn tempids [es]
  (zipmap es (->> (iterate inc 0) (map str))))

#?(:clj
   (defn clone-entities
     "Clone a Datomic entity and any reachable entities by walking the entity graph breadth-first with Loom."
     [$ es]
     (let [traverse #(loom/bf-traverse (partial datomic-entity-successors $) %)
           reachable-es (set (mapcat traverse es))]
       (->> (seq reachable-es)
            (d/pull-many $ ['*])
            (mapv (partial alter-ids (tempids reachable-es)))))))

;(defn lookup-id [lookup id]
;  (if-let [tid (get-in lookup [:lookup id])]
;    [lookup tid]
;    (let [tid (str id)]
;      [(assoc-in lookup [:lookup id] tid)
;       tid])))

#?(:clj
   (defn export-schema [$]
     (->> (d/q '[:find [?attr ...] :where
                 [:db.part/db :db.install/attribute ?attr]
                 [(> ?attr 62)]] $)
          (d/pull-many $ ['*])
          (sort-by :db/ident))))
