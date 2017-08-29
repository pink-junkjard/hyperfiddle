(ns hypercrud.client.schema
  (:require [cats.core :as cats]
            [hypercrud.client.core :as hc]
            [hypercrud.types.QueryRequest :refer [->QueryRequest]]
            [hypercrud.ui.form-util :as form-util]
            [hypercrud.util.core :as util]))

(defn hc-attr-request [root-dbval]
  (->QueryRequest '[:find ?attr :in $ :where
                    (or [?attr :attribute/renderer]
                        [?attr :attribute/hc-type])]
                  {"$" root-dbval}
                  {"?attr" [root-dbval [:attribute/ident
                                        :attribute/renderer
                                        {:attribute/hc-type ['*]}]]}))

(defn schema-request [dbval]
  (->QueryRequest '[:find ?attr :in $ :where [?attr :db/ident]]
                  {"$" dbval}
                  {"?attr" [dbval ['*
                                   {:db/valueType [:db/id :db/ident]
                                    :db/cardinality [:db/id :db/ident]
                                    :db/unique [:db/id :db/ident]}]]}))

(defn schema-requests-for-link [link query-params param-ctx]
  (->> (form-util/get-ordered-find-elements link query-params param-ctx)
       (map #(form-util/fe->db % param-ctx))
       (map schema-request)
       ; this hc-attr-request will eventually fall out
       ; until then, we still need to join in renderer and hc-type to the datomic schema
       (concat [(hc-attr-request (:root-db param-ctx))])))

(defn hydrate-schema [link query-params param-ctx]
  (-> (hc/hydrate (:peer param-ctx) (hc-attr-request (:root-db param-ctx)))
      (cats/bind
        (fn [root-data]
          (let [indexed-root (->> root-data
                                  (mapv #(get % "?attr"))
                                  (util/group-by-assume-unique :attribute/ident)
                                  (util/map-values #(dissoc % :attribute/ident :db/id)))]
            (->> (form-util/get-ordered-find-elements link query-params param-ctx)
                 (mapv (fn [fe]
                         (->> (form-util/fe->db fe param-ctx)
                              (schema-request)
                              (hc/hydrate (:peer param-ctx))
                              (cats/fmap (fn [schema]
                                           [(:find-element/name fe)
                                            (->> schema
                                                 (mapv #(get % "?attr"))
                                                 (util/group-by-assume-unique :db/ident)
                                                 (merge-with merge indexed-root))])))))
                 (cats/sequence)
                 (cats/fmap #(into {} %))))))))
