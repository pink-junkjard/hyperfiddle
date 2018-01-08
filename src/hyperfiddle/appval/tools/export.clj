(ns hyperfiddle.appval.tools.export
  (:require [datomic.api :as d]
            [hypercrud.server.datomic.root-init :as root-init]
            [hypercrud.util.core :as util]
            [loom.alg-generic :as loom]))


(defn root-attr? [attr]
  (not (.startsWith (namespace (:db/ident attr)) "zzz.")))

(defn lookup-id [lookup id]
  (if-let [tid (get-in lookup [:lookup id])]
    [lookup tid]
    (let [tid (str id)]
      [(assoc-in lookup [:lookup id] tid)
       tid])))

(defn clone-entity [hydrate attr-filter [entities lookup] entity-id]
  (if (get entities entity-id)
    [entities lookup]
    (let [[lookup tempid] (lookup-id lookup entity-id)

          [[entities lookup] entity]
          (->> (hydrate entity-id)
               (into {})
               (util/map-keys hydrate)
               (filter #(attr-filter (first %)))
               (reduce (fn [[[entities lookup] entity] [{:keys [:db/ident :db/valueType :db/cardinality]} v]]
                         (cond
                           (and (= :db.type/ref valueType) (= cardinality :db.cardinality/one))
                           (let [[lookup tempid] (lookup-id lookup (:db/id v))]
                             [[entities lookup] (assoc entity ident {:db/id tempid})])

                           (and (= :db.type/ref valueType) (= cardinality :db.cardinality/many))
                           (let [[[entities lookup] v] (reduce (fn [[[entities lookup] vs] v]
                                                                 (let [[lookup tempid] (lookup-id lookup (:db/id v))]
                                                                   [[entities lookup] (conj vs {:db/id tempid})]))
                                                               [[entities lookup] #{}]
                                                               v)]
                             [[entities lookup] (assoc entity ident v)])

                           :else
                           [[entities lookup] (assoc entity ident v)]))
                       [[entities lookup] {:db/id tempid}]))]
      [(assoc entities entity-id entity) lookup])))

(defn export-data [conn start-node attr-filter include-starting-node?]
  (let [db (d/db conn)
        hydrate #(d/touch (d/entity db %))
        successors (fn [node]
                     (->> (if (map? node) node (hydrate node))
                          (into {})
                          (util/map-keys hydrate)
                          (filter #(attr-filter (first %)))
                          (mapcat (fn [[{:keys [:db/valueType :db/cardinality]} v]]

                                    (cond
                                      (and (= :db.type/ref valueType) (= cardinality :db.cardinality/one))
                                      [(:db/id v)]

                                      (and (= :db.type/ref valueType) (= cardinality :db.cardinality/many))
                                      (mapv :db/id v)

                                      :else
                                      nil)))))]
    (->> (loom/bf-traverse successors start-node)
         (drop (if include-starting-node? 0 1))
         (reduce (partial clone-entity hydrate attr-filter) [{} {:lookup {}}])
         first
         vals)))

(defn export-links [source-conn]
  (let [link {:fiddle/links (->> (d/q '[:find [?link ...] :in $ :where
                                        (or [?link :fiddle/name]
                                            [?link :fiddle/type])]
                                      (d/db source-conn))
                                 (map (fn [link-id] {:db/id link-id})))}]
    (export-data source-conn link root-attr? false)))

(defn export-renderers [source-conn]
  (let [$ (d/db source-conn)]
    (->> (d/q '[:find ?ident ?renderer :in $ :where
                [?attr :attribute/ident ?ident]
                [?attr :attribute/renderer ?renderer]]
              $)
         (mapcat (fn [[ident renderer]]
                   [[:db/add (str (hash ident)) :attribute/ident ident]
                    [:db/add (str (hash ident)) :attribute/renderer renderer]])))))

(defn export-schema [conn]
  (->> (root-init/reflect-schema conn)
       (sort-by :db/ident)
       (mapv (partial into {}))))

(defn export-source-code-schema [conn]
  (->> (export-schema conn)
       (filter root-attr?)))

(defn export-all [uri schema-file link-file]
  (let [conn (d/connect uri)]
    (spit schema-file (export-schema conn))
    (spit link-file (export-links conn))
    (assert false "what about renderers?")))

(defn export-hyperfiddle [root-conn schema-file link-file]
  (spit schema-file (export-source-code-schema root-conn))
  (spit link-file (export-links root-conn))
  (assert false "what about renderers?"))
