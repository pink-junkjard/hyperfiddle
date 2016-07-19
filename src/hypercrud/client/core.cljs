(ns hypercrud.client.core
  (:refer-clojure :exclude [update])
  (:require [cljs.core.match :refer-macros [match]]
            [goog.Uri]
            [hypercrud.client.fetch :as fetch]
            [hypercrud.client.tx :as tx]
            [promesa.core :as p]))


(defprotocol Graph
  (select [this named-query])
  (entity [this eid])
  (with [this more-statements]))


(defprotocol Hypercrud
  ;;(authenticate! [this username password])     ; sets a http cookie with a token
  (whoami [this])                                           ; read the cookie
  (graph [this])
  (enter! [this graph-dependencies])
  (transact! [this tx]))


(defn map-values [f m]
  (->> m
       (map (fn [[k v]]
              [k (f v)]))
       (into {})))


(deftype HypercrudGraph [schema statements resultsets local-statements]
  Graph
  (select [this named-query]
    (get resultsets named-query))


  (entity [this eid]
    (tx/apply-statements-to-entity
      schema
      (concat statements local-statements)
      {:db/id eid}))


  (with [this more-statements]
    (HypercrudGraph.
      schema statements resultsets (concat local-statements more-statements)))

  IHash
  (-hash [this]
    (hash (map hash [schema statements resultsets local-statements])))

  IEquiv
  (-equiv [this other]
    (= (hash this) (hash other))))


(defn build-hc-graph [schema pulled-trees-map]
  (let [pulled-trees (apply concat (vals pulled-trees-map)) ;mash query results together
        statements (mapcat #(tx/pulled-tree-to-statements schema %) pulled-trees)
        resultsets (map-values #(map :db/id %) pulled-trees-map)]
    (HypercrudGraph. schema statements resultsets [])))


(deftype HypercrudClient [user-token entry-uri schema ^:mutable graph]
  Hypercrud
  (enter! [this graph-dependencies]
    (-> (fetch/fetch! user-token entry-uri (goog.Uri. "/api/enter") graph-dependencies)
        (p/then #(set! graph (build-hc-graph schema (-> % :body :hypercrud))))))

  (whoami [this] "dustin")

  (graph [this]
    (assert (not= nil graph) "invariant - runtime must call enter! first")
    graph)

  (transact! [this tx]
    (fetch/transact! user-token entry-uri tx)))



(let [state (atom 0)]
  (defn tempid! []
    (swap! state dec)
    @state))
