(ns hypercrud.browser.core
  (:require [cljs.reader :as reader]
            [clojure.string :as string]
            [hypercrud.browser.base-64-url-safe :as base64]
            [hypercrud.browser.pages.field :as field]
            [hypercrud.browser.pages.index :as index]
            [hypercrud.browser.pages.query :as query]
            [hypercrud.client.core :as hc]
            [hypercrud.client.internal :as internal]
            [hypercrud.types :refer [->DbId ->DbVal]]
            [hypercrud.util :as util]))


(defn route [page-rel-path {:keys [query-fn field-fn index-fn else]}]
  (let [path-params (string/split page-rel-path "/")]
    (cond
      (and (= 1 (count path-params)) (= (first path-params) ""))
      (index-fn)

      (and (= 1 (count path-params)) (not (empty? (first path-params))))
      (query-fn (reader/read-string (base64/decode (first path-params))))

      (and (= (second path-params) "field") (= 3 (count path-params)))
      (let [[field-dbid _ dbid] path-params
            field-dbid (reader/read-string (base64/decode field-dbid))
            dbid (reader/read-string (base64/decode dbid))]
        ;todo this should accept a real entity type
        (field-fn dbid field-dbid))

      :else (else))))


(defn ui [links editor-graph stage-tx! graph page-rel-path navigate-cmp param-ctx debug]
  [:div.browser {:class debug}
   (route page-rel-path
          {:query-fn (fn [{:keys [link-dbid] :as params-map}]
                       (let [link (hc/entity editor-graph link-dbid)]
                         (query/ui stage-tx! graph link params-map navigate-cmp param-ctx debug)))
           :field-fn (fn [dbid field-dbid]
                       (let [entity-dbval (->DbVal (.-conn-id dbid) nil)
                             entity (hc/entity (hc/get-dbgraph graph entity-dbval) dbid)
                             field (hc/entity editor-graph field-dbid)]
                         (field/ui stage-tx! graph entity field navigate-cmp)))
           :index-fn #(index/ui links navigate-cmp param-ctx)
           :else (constantly [:div "no route for: " page-rel-path])})])


(defn query [super-graph editor-graph page-rel-path param-ctx debug]
  (route page-rel-path
         {:query-fn (fn [{:keys [link-dbid] :as params-map}]
                      (let [link (hc/entity editor-graph link-dbid)]
                        (query/query super-graph link params-map param-ctx debug)))
          :field-fn (fn [dbid field-dbid]
                      (let [field (hc/entity editor-graph field-dbid)]
                        (field/query dbid field param-ctx)))
          :index-fn #(index/query)
          :else (constantly {})}))


(defn replace-tempids-in-path [page-rel-path tempid-lookup]
  (let [replace-tempid #(or (get tempid-lookup %) %)
        process-query-url (fn [params-map]
                            (-> params-map
                                (update :link-dbid replace-tempid)
                                (update :query-params #(util/map-values replace-tempid %))
                                (update :create-new-find-elements (fn [old-map]
                                                                    ; generate a new tempid for each existing tempid
                                                                    ; we don't want to replace here
                                                                    (util/map-values
                                                                      #(hc/*temp-id!* (:conn-id %))
                                                                      old-map)))
                                pr-str
                                base64/encode))
        process-field-url (fn [dbid field-dbid]
                            (str (base64/encode (pr-str (replace-tempid field-dbid)))
                                 "/field/"
                                 (base64/encode (pr-str (replace-tempid dbid)))))]
    (route page-rel-path {:query-fn process-query-url
                          :field-fn process-field-url
                          :index-fn (constantly page-rel-path)
                          :else (constantly page-rel-path)})))