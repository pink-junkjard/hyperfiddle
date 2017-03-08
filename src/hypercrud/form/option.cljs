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

(defn render-label [v]                                      ; hardcode some stuff
  (cond
    (instance? cljs.core/Keyword v) (name v)
    :else (str v))
  #_(condp (fn [x c] (instance? c x)) v
      cljs.core/Keyword (name v)
      (str v)))

(defn build-label [colspec result param-ctx]
  (->> (partition 3 colspec)
       (mapv (fn [[fe-name ident maybe-field]]
               (let [value (get-in result [fe-name ident])]
                 ; Custom label renderers? Can't use the attribute renderer, since that
                 ; is how we are in a select options in the first place.
                 (render-label value))))
       (interpose ", ")
       (apply str)))

(defn get-hydrate-key [field]
  (hash (-> field :field/options-anchor :anchor/link :link/request)))


(defn hydrate-options [field param-ctx]                     ; needs to return options as [[:db/id label]]
  (assert field)
  ; we are assuming we have a query link here
  (let [link (-> field :field/options-anchor :anchor/link)
        request (-> link :link/request)]
    (mlet [q (if-let [qstr (:link-query/value request)]     ; We avoid caught exceptions when possible
               (exception/try-on (reader/read-string qstr))
               (exception/failure nil))                     ; is this a success or failure? Doesn't matter - datomic will fail.
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
                             [(:db/id entity) (build-label colspec relation param-ctx)])))))))))
