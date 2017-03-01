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
  (let [link (-> field :field/options-anchor :anchor/link)
        request (-> link :link/request)]
    (mlet [q (exception/try-on (reader/read-string (:link-query/value request)))
           result (let [params-map (merge (:query-params (links/build-url-params-map (:field/options-anchor field) param-ctx))
                                          (q-util/build-dbhole-lookup request))
                           query-value (q-util/query-value q request params-map param-ctx)]
                       (hc/hydrate (:peer param-ctx) query-value))]
          (let [colspec (form-util/determine-colspec result link param-ctx)]
            (cats/return
              (->> result
                   (mapv (fn [relation]
                           (let [[fe-name ident maybe-field] (first (partition 3 colspec))
                                 entity (get relation fe-name)]
                             [(:db/id entity) (build-label colspec relation)])))))))))
