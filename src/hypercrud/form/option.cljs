(ns hypercrud.form.option
  (:require [cats.core :as cats :refer-macros [mlet]]
            [cats.monad.exception :as exception]
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
  (assert field)
  ; we are assuming we have a query link here
  (let [link-query (-> field :field/options-anchor :anchor/link :link/request)]
    (mlet [q (exception/try-on (reader/read-string (:link-query/value link-query)))
           resultset (let [params-map (merge (:query-params (links/build-url-params-map (:field/options-anchor field) param-ctx))
                                             (q-util/build-dbhole-lookup link-query))
                           query-value (q-util/query-value q link-query params-map param-ctx)]
                       (hc/hydrate (:peer param-ctx) query-value))]
          (let [ordered-find-elements (find-elements-util/order-find-elements (:link-query/find-element link-query) q)]
            (cats/return (mapv (fn [result]
                                 (mapv (fn [find-element]
                                         (get result (:find-element/name find-element)))
                                       ordered-find-elements))
                               resultset))))))
