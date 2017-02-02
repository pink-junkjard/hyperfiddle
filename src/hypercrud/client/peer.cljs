(ns hypercrud.client.peer
  (:require [cats.monad.exception :as exception]
            [goog.Uri]
            [hypercrud.client.core :as hc]
            [hypercrud.client.schema :as schema-util]
            [hypercrud.types :as types :refer [->DbId ->DbVal ->DbError]]
            [hypercrud.util :as util]))


(deftype Peer [requests dbval->schema pulled-trees-map resultsets]
  hc/Peer
  (hydrate [this request]
    (if-let [resultset-or-error (get resultsets request)]
      (if (instance? types/DbError resultset-or-error)
        (exception/failure (js/Error. (.-msg resultset-or-error))) ;build a stack trace
        (exception/success resultset-or-error))
      (exception/failure (js/Error. (str "Unhydrated request:\n" (pr-str request))))))


  (t [this]
    (hash (keys dbval->schema)))


  IHash
  (-hash [this]
    (hash requests))


  IEquiv
  (-equiv [this other]
    (= (hash this) (hash other))))


(defn ->peer [editor-schema requests pulled-trees-map]
  (let [editor-dbval (->DbVal hc/*root-conn-id* nil)
        schema-request-lookup (->> requests
                                   (filter #(instance? types/DbVal %))
                                   (mapv (juxt #(schema-util/schema-request editor-dbval %) identity))
                                   (into {}))
        resultset-by-request (merge pulled-trees-map
                                    (-> (util/map-keys #(get schema-request-lookup %) pulled-trees-map)
                                        (dissoc nil)))
        dbval->schema (->> requests
                           (filter #(instance? types/DbVal %))
                           (remove #(= % editor-dbval))
                           (mapv (juxt identity #(->> (let [resultset-or-error (get resultset-by-request %)]
                                                        (if (instance? types/DbError resultset-or-error)
                                                          (js/Error. (.-msg resultset-or-error))
                                                          resultset-or-error))
                                                      (mapv (fn [result] (get result "?attr")))
                                                      (schema-util/build-schema))))
                           (into {editor-dbval editor-schema}))]
    (->Peer requests dbval->schema pulled-trees-map resultset-by-request)))
