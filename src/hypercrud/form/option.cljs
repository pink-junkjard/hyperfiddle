(ns hypercrud.form.option
  (:require [cats.monad.exception :as exception]
            [cljs.reader :as reader]
            [hypercrud.browser.links :as links]
            [hypercrud.client.core :as hc]
            [hypercrud.form.find-elements :as find-elements-util]
            [hypercrud.form.q-util :as q-util]
            [hypercrud.types :refer [->DbVal]]))


(defn label-prop [field result]
  ; we are assuming we have a query link here
  (let [find-elements (-> field :field/options-anchor :anchor/link :link/request :link-query/find-element)]
    (->> (mapcat (fn [{{fields :form/field} :find-element/form} entity]
                   (->> fields
                        (mapv #(-> % :field/attribute :attribute/ident))
                        (mapv #(get entity %))))
                 find-elements result)
         (interpose ", ")
         (apply str))))


(defn get-key [field]
  (hash (-> field :field/options-anchor :anchor/link :link/request)))


(defn get-option-records [field param-ctx]
  ; we are assuming we have a query link here
  (let [{find-elements :link-query/find-element q :link-query/value :as link-query} (-> field :field/options-anchor :anchor/link :link/request)]
    (if-let [q (if-not (empty? q)
                 (reader/read-string q))]
      (let [ordered-find-elements (find-elements-util/order-find-elements find-elements q)
            result-query (let [params-map (merge (:query-params (links/build-url-params-map (:field/options-anchor field) param-ctx))
                                                 (q-util/build-dbhole-lookup link-query))]
                           (q-util/query-value q link-query params-map param-ctx))]
        (->> (hc/hydrate (:peer param-ctx) result-query)
             (exception/extract)
             (mapv (fn [result]
                     (mapv (fn [find-element]
                             (get result (:find-element/name find-element)))
                           ordered-find-elements))))))))
