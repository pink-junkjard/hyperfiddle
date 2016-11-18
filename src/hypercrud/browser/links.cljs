(ns hypercrud.browser.links
  (:require [cljs.reader :as reader]
            [hypercrud.browser.base-64-url-safe :as base64]
            [hypercrud.client.internal :as internal]
            [hypercrud.form.q-util :as q-util]
            [hypercrud.client.core :as hc]))


(defn field-link [field-dbid dbid f]
  ;; field-dbid is assumed to be the editor-graph connection
  (f (str (.-id field-dbid) "/field/" (.-id dbid) "/" (base64/encode (internal/transit-encode (.-conn-id dbid))))))


(defn query-link [link param-ctx f]
  (let [empty-result-lookup (let [lookup (:link/if-empty-result link)]
                              (if-not (empty? lookup)
                                (reader/read-string lookup)))
        query-params (q-util/build-params-map link param-ctx)
        data {:query-params query-params
              ;; Create a result of shape [?e ?f] with new entities colored
              :create-new-find-elements (->> (:link/find-element link)
                                             (mapv (juxt :find-element/name (fn [{:keys [:find-element/connection :find-element/name]}]
                                                                              ;; because we don't yet send local-datoms up to datomic,
                                                                              ;; we may need to adjust the query results to account for local datoms.
                                                                              ;; This is not possible to do generally without DataScript but we special
                                                                              ;; case the create-new case because users need it
                                                                              (if-let [provided-val (get query-params (get empty-result-lookup name))]
                                                                                provided-val
                                                                                ; todo use the find-element connection's dbval
                                                                                (hc/*temp-id!* (.-conn-id (get param-ctx :dbval)))))))
                                             (into {}))}]
    ;; link-dbid is assumed to be the editor-graph connection
    (f (str (-> link .-dbid .-id) "/" (base64/encode (pr-str data))))))
