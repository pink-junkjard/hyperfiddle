(ns hypercrud.browser.core
  (:require [cljs.reader :as reader]
            [clojure.string :as string]
            [hypercrud.browser.base-64-url-safe :as base64]
            [hypercrud.browser.query :as query]
            [hypercrud.client.core :as hc]
            [hypercrud.client.internal :as internal]
            [hypercrud.types :refer [->DbId ->DbVal]]
            [hypercrud.util :as util]))


(defn route [page-rel-path {:keys [query-fn else]}]
  (let [path-params (string/split page-rel-path "/")]
    (cond
      (and (= 1 (count path-params)) (not (empty? (first path-params))))
      (query-fn (reader/read-string (base64/decode (first path-params))))

      :else (else))))


(defn ui [page-rel-path {:keys [meta-graph] :as param-ctx}]
  (route page-rel-path
         {:query-fn (fn [{:keys [link-dbid] :as params-map}]
                      (let [link (hc/entity meta-graph link-dbid)]
                        (query/ui link params-map param-ctx)))
          :else (constantly [:div "no route for: " page-rel-path])}))


(defn query [page-rel-path {:keys [meta-graph] :as param-ctx}]
  (route page-rel-path
         {:query-fn (fn [{:keys [link-dbid] :as params-map}]
                      (let [link (hc/entity meta-graph link-dbid)]
                        (query/query link params-map param-ctx)))
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
                                base64/encode))]
    (route page-rel-path {:query-fn process-query-url
                          :else (constantly page-rel-path)})))