(ns hypercrud.server.util.datomic-adapter
  (:require [datomic.api :as d]
            [hypercrud.types.DbId :refer [->DbId]])
  (:import (hypercrud.types.DbId DbId)))


;; hypercrud.client doesn't have an entity type and can't understand datomic's
;; syntax sugar around the entity type so we desugar some entity related things here.

(defn hc-tempid? [eid] (string? eid))

(defn datomic-tempid? [e]                                   ; these types are loose af in datomic wtf
  (if (= (class e) datomic.db.DbId)                         ; all tempids are instance of this type by now?
    (> 0 (.idx e))                                          ; always tempid instance
    (do
      (assert (< 0 e)) true)))                              ; non-tempids are always positive long?

(defn did->hid [id]
  (condp = (class id)
    datomic.db.DbId (let [o (.idx id)]
                      (if (< o 0) (str o) o))
    Long (if (< id 0) (str id) id)
    String (assert false "unimplemented")))

(defn dbid->datomic-id [dbid]
  #_(.id dbid)                                              ; want this, but string tempids are bust so convert
  (let [id (.id dbid)]
    (if (hc-tempid? id)
      (d/tempid :db.part/user (Long/parseLong id))          ; this is the line that poisons types everywhere
      id)))

(defn process-add-ret [[op e a v]]
  [op (dbid->datomic-id e) a (if (instance? DbId v)
                               (dbid->datomic-id v)
                               v)])

(defn stmt-dbid->id [[op e a v :as stmt]]
  (case op
    :db/add (process-add-ret stmt)
    :db/retract (process-add-ret stmt)
    :db.fn/retractEntity [op (.id e)]
    (throw (new Error (str "Unable to process op: " op)))))

;(defn datomic-map-to-hc-stmt [maps conn-id]
;  (->> maps
;       (mapcat (fn [m]
;                 (let [dbid (->DbId (-> m :db/id .idx) conn-id)
;                       ref->v (fn [v] (condp = (class v)
;                                        datomic.db.DbId (->DbId (.idx v) conn-id)
;                                        v))]
;                   (->> (seq m)
;                        (filter (fn [[k v]] (not= :db/id k)))
;                        (mapcat (fn [[attr val]]
;                                  (if (coll? (ref->v val))
;                                    (mapv (fn [val] [:db/add dbid attr (ref->v val)]) val)
;                                    [[:db/add dbid attr (ref->v val)]])))))))))
