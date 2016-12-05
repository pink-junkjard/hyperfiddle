(ns hypercrud.browser.core
  (:require [cljs.reader :as reader]
            [clojure.string :as string]
            [hypercrud.browser.base-64-url-safe :as base64]
            [hypercrud.browser.pages.field :as field]
            [hypercrud.browser.pages.index :as index]
            [hypercrud.browser.pages.query :as query]
            [hypercrud.client.core :as hc]
            [hypercrud.client.internal :as internal]
            [hypercrud.types :refer [->DbId ->DbVal]]))


(defn route [page-rel-path {:keys [query-fn field-fn index-fn else]}]
  (let [path-params (string/split page-rel-path "/")]
    (cond
      (= 2 (count path-params))
      (let [[link-id params] path-params
            link-id (js/parseInt link-id 10)                ;todo dbid
            params-map (reader/read-string (base64/decode params))]
        (query-fn link-id params-map))

      (and (= (second path-params) "field") (= 4 (count path-params)))
      (let [[field-id _ id conn-id] path-params
            field-id (js/parseInt field-id)
            conn-id (internal/transit-decode (base64/decode conn-id))
            dbid (->DbId (js/parseInt id 10) conn-id)
            dbval (->DbVal conn-id nil)]
        ;todo this should accept a real entity type
        (field-fn dbval dbid field-id))

      (and (= (first path-params) "") (= 1 (count path-params)))
      (index-fn)

      :else (else))))


(defn ui [links editor-graph stage-tx! graph page-rel-path navigate-cmp param-ctx debug]
  [:div.browser {:class debug}
   (route page-rel-path
          {:query-fn (fn [link-id params-map]
                       (let [link (hc/entity editor-graph (->DbId link-id (-> editor-graph .-dbval .-conn-id)))]
                         (query/ui stage-tx! graph link params-map navigate-cmp param-ctx debug)))
           :field-fn (fn [dbval dbid field-id]
                       (let [entity (hc/entity (hc/get-dbgraph graph dbval) dbid)
                             field (hc/entity editor-graph (->DbId field-id (-> editor-graph .-dbval .-conn-id)))]
                         (field/ui stage-tx! graph entity field navigate-cmp)))
           :index-fn #(index/ui links navigate-cmp param-ctx)
           :else (constantly [:div "no route for: " page-rel-path])})])


(defn query [editor-graph page-rel-path param-ctx]
  (route page-rel-path
         {:query-fn (fn [link-id params-map]
                      (let [link (hc/entity editor-graph (->DbId link-id (-> editor-graph .-dbval .-conn-id)))]
                        (query/query editor-graph link params-map param-ctx)))
          :field-fn (fn [dbval dbid field-id]
                      (let [field (hc/entity editor-graph (->DbId field-id (-> editor-graph .-dbval .-conn-id)))]
                        (field/query dbid field param-ctx)))
          :index-fn #(index/query)
          :else (constantly {})}))
