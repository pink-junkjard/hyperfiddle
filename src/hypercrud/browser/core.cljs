(ns hypercrud.browser.core
  (:require [cljs.reader :as reader]
            [clojure.string :as string]
            [hypercrud.browser.base-64-url-safe :as base64]
            [hypercrud.browser.pages.entity :as entity]
            [hypercrud.browser.pages.field :as field]
            [hypercrud.browser.pages.query :as query]
            [hypercrud.client.core :as hc]
            [hypercrud.client.internal :as internal]
            [hypercrud.types :refer [->DbId ->DbVal]]))


(defn route [page-rel-path {:keys [query-fn entity-fn field-fn else]}]
  (let [path-params (string/split page-rel-path "/")]
    (cond
      (and (= (second path-params) "query") (= 4 (count path-params)))
      (let [[form-id _ query-id params] path-params
            form-id (js/parseInt form-id 10)                ;todo dbid
            query-id (js/parseInt query-id 10)
            params (reader/read-string (base64/decode params))]
        (query-fn form-id query-id params))

      (and (= (second path-params) "entity") (= 4 (count path-params)))
      (let [[form-id _ id conn-id] path-params
            form-id (js/parseInt form-id 10)
            conn-id (internal/transit-decode (base64/decode conn-id))
            dbid (->DbId (js/parseInt id 10) conn-id)
            dbval (->DbVal conn-id nil)]
        ;todo this should accept a real entity type
        (entity-fn dbval dbid form-id))

      (and (= (second path-params) "field") (= 4 (count path-params)))
      (let [[field-id _ id conn-id] path-params
            field-id (js/parseInt field-id)
            conn-id (internal/transit-decode (base64/decode conn-id))
            dbid (->DbId (js/parseInt id 10) conn-id)
            dbval (->DbVal conn-id nil)]
        ;todo this should accept a real entity type
        (field-fn dbval dbid field-id))

      :else (else))))


(defn ui [cur editor-graph stage-tx! graph page-rel-path navigate! navigate-cmp param-ctx]
  (route page-rel-path
         {:query-fn (fn [form-id query-id params]
                      (let [form (hc/entity editor-graph (->DbId form-id (-> editor-graph .-dbval .-conn-id)))
                            query (hc/entity editor-graph (->DbId query-id (-> editor-graph .-dbval .-conn-id)))]
                        (query/ui cur editor-graph stage-tx! graph form query params navigate! navigate-cmp param-ctx)))
          :entity-fn (fn [dbval dbid form-id]
                       (let [entity (hc/entity (hc/get-dbgraph graph dbval) dbid)
                             form (hc/entity editor-graph (->DbId form-id (-> editor-graph .-dbval .-conn-id)))]
                         (entity/ui cur stage-tx! graph entity form navigate! navigate-cmp param-ctx)))
          :field-fn (fn [dbval dbid field-id]
                      (let [entity (hc/entity (hc/get-dbgraph graph dbval) dbid)
                            field (hc/entity editor-graph (->DbId field-id (-> editor-graph .-dbval .-conn-id)))]
                        (field/ui cur stage-tx! graph entity field navigate-cmp)))
          :else (constantly [:div "no route for: " page-rel-path])}))


(defn query [state editor-graph page-rel-path param-ctx]
  (route page-rel-path
         {:query-fn (fn [form-id query-id params]
                      (let [form (hc/entity editor-graph (->DbId form-id (-> editor-graph .-dbval .-conn-id)))
                            query (hc/entity editor-graph (->DbId query-id (-> editor-graph .-dbval .-conn-id)))]
                        (query/query state editor-graph query params form param-ctx)))
          :entity-fn (fn [dbval dbid form-id]
                       (let [form (hc/entity editor-graph (->DbId form-id (-> editor-graph .-dbval .-conn-id)))]
                         (entity/query state dbid form param-ctx)))
          :field-fn (fn [dbval dbid field-id]
                      (let [field (hc/entity editor-graph (->DbId field-id (-> editor-graph .-dbval .-conn-id)))]
                        (field/query state dbid field param-ctx)))
          :else (constantly {})}))
