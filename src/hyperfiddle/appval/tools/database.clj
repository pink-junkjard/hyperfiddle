(ns hyperfiddle.appval.tools.database
  (:require [datomic.api :as d]
            [hyperfiddle.appval.tools.export :as export]
            [hypercrud.util.core :as util]))


(defn empty-db? [uri]
  (or (= 63 (d/basis-t (d/db (d/connect uri))))
      (let [conn (d/connect uri)]
        (->> (export/export-schema conn)
             (map :db/ident)
             (d/q '[:find [?e ...] :in $ [?attr ...] :where
                    [?e ?attr ?v]]
                  (d/db conn))
             empty?))))

(defn list-empty-dbs [transactor-uri]
  (->> (d/get-database-names (str transactor-uri "*"))
       (map #(str transactor-uri %))
       (filter empty-db?)))

; https://stackoverflow.com/a/25389808/959627
(defn rollback [conn tx]
  (let [tx-log (-> conn d/log (d/tx-range tx nil) first)    ; find the transaction
        txid (-> tx-log :t d/t->tx)                         ; get the transaction entity id
        newdata (->> (:data tx-log)                         ; get the datoms from the transaction
                     (remove #(= (:e %) txid))              ; remove transaction-metadata datoms
                     ; invert the datoms add/retract state.
                     (map #(do [(if (:added %) :db/retract :db/add) (:e %) (:a %) (:v %)]))
                     reverse)]                              ; reverse order of inverted datoms.
    newdata))

(defn entity-history [conn eid]
  (let [db (d/db conn)
        $ (d/history db)
        hydrate #(d/touch (d/entity db %))]
    (->> (d/q '[:find ?a ?v ?tx ?x :in $ ?e :where
                [?e ?a ?v ?tx ?x]]
              $ eid)
         (map #(assoc % 0 (:db/ident (hydrate (get % 0)))))
         (group-by #(nth % 2))
         (util/map-values #(sort-by first %))
         (sort-by first))))
