(ns hyperfiddle.project
  (:require
    [cats.core :as cats]
    [contrib.reactive :as r]
    [hypercrud.client.core :as hc]
    [hypercrud.types.QueryRequest :refer [->QueryRequest]]))


(defn attrs-request [ctx]
  (->QueryRequest '[:find [(pull ?attr [:attribute/ident :attribute/renderer]) ...]
                    :where [?attr :attribute/ident]]
                  [(hc/db (:peer ctx) (get-in ctx [:hypercrud.browser/domain :domain/fiddle-database :database/uri]) (:branch ctx))]))

(let [f (fn [attrs]
          (->> attrs
               (map (juxt :attribute/ident :attribute/renderer))
               (into {})))]
  (defn hydrate-attrs [ctx]
    (r/fmap->> (hc/hydrate (:peer ctx) (:branch ctx) (attrs-request ctx))
               (cats/fmap f))))
