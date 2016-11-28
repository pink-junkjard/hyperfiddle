(ns hypercrud.browser.links
  (:require [cljs.reader :as reader]
            [hypercrud.browser.base-64-url-safe :as base64]
            [hypercrud.client.internal :as internal]
            [hypercrud.compile.eval :refer [eval]]
            [hypercrud.form.q-util :as q-util]
            [hypercrud.client.core :as hc]))


(defn field-link [field-dbid dbid]
  ;; field-dbid is assumed to be the editor-graph connection
  {:href (str (.-id field-dbid) "/field/" (.-id dbid) "/" (base64/encode (internal/transit-encode (.-conn-id dbid))))})


(defn query-link [stage-tx! link param-ctx]
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
                                             (into {}))}
        tx-fn (if-let [tx-fn (:link/tx-fn link)]
                (let [{value :value error :error} (eval tx-fn)]
                  ;; non-fatal error, report it here so user can fix it
                  (if error (js/alert (str "cljs eval error: " error)))
                  value))]
    ;; link-dbid is assumed to be the editor-graph connection

    ;; add-result #(tx/edit-entity (:db/id entity) ident [] [(first %)])
    (if tx-fn
      {:on-click #(stage-tx! (tx-fn param-ctx))}
      {:href (str (-> link .-dbid .-id) "/" (base64/encode (pr-str data)))})))
