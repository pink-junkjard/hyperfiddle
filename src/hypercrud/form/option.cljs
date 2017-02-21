(ns hypercrud.form.option
  (:require [cats.core :as cats :refer-macros [mlet]]
            [cats.monad.exception :as exception]
            [cljs.reader :as reader]
            [hypercrud.browser.links :as links]
            [hypercrud.client.core :as hc]
            [hypercrud.form.find-elements :as find-elements-util]
            [hypercrud.form.q-util :as q-util]
            [hypercrud.types :refer [->DbVal]]
            [hypercrud.ui.form-util :as form-util]))


(defn build-label [colspec result]
  (->> (partition 3 colspec)
       (mapv (fn [[fe-name ident maybe-field]]
               (get-in result [fe-name ident])))
       (interpose ", ")
       (apply str)))

(defn get-hydrate-key [field]
  (hash (-> field :field/options-anchor :anchor/link :link/request)))


(defn hydrate-options [field param-ctx]                     ; needs to return options as [[:db/id label]]
  (assert field)
  ; we are assuming we have a query link here
  (let [request (-> field :field/options-anchor :anchor/link :link/request)]
    (mlet [q (exception/try-on (reader/read-string (:link-query/value request)))
           resultset (let [params-map (merge (:query-params (links/build-url-params-map (:field/options-anchor field) param-ctx))
                                             (q-util/build-dbhole-lookup request))
                           query-value (q-util/query-value q request params-map param-ctx)]
                       (hc/hydrate (:peer param-ctx) query-value))]
          (let [ordered-find-elements (find-elements-util/order-find-elements (:link-query/find-element request) q)
                colspec (form-util/determine-colspec resultset ordered-find-elements param-ctx)]
            (cats/return
              (->> resultset
                   (mapv (fn [result]
                           (assert (= 1 (count ordered-find-elements)) "Cannot use multiple find-elements for an options-link; because you can't persist a ref to a tuple")
                           (let [entity (get result (-> ordered-find-elements first :find-element/name))]
                             [(:db/id entity) (build-label colspec result)])))))))))
