(ns hypercrud.browser.query
  (:require [cljs.pprint :as pprint]
            [cljs.reader :as reader]
            [clojure.set :as set]
            [hypercrud.browser.links :as links]
            [hypercrud.client.core :as hc]
            [hypercrud.compile.eval :refer [eval]]
            [hypercrud.form.find-elements :as find-elements-util]
            [hypercrud.form.q-util :as q-util]
            [hypercrud.types :refer [->DbId ->DbVal ->Entity]]
            [hypercrud.ui.auto-control :refer [auto-control]]
            [hypercrud.ui.form :as form]
            [hypercrud.ui.table :as table]))


(defn initial-params-map [q params]
  (select-keys params (q-util/parse-holes q)))


(defn holes-filled? [hole-names param-values]
  (set/subset? (set hole-names) (set (keys (into {} (remove (comp nil? val) param-values))))))


(defn repeating-links [stage-tx! link result navigate-cmp param-ctx]
  (->> (:link/link link)
       (filter :link/repeating?)
       (filter #(nil? (:link/field %)))
       (mapv (fn [link]
               (let [param-ctx (merge param-ctx {:result result})
                     props (links/query-link stage-tx! link param-ctx)]
                 ^{:key (:db/id link)}
                 [navigate-cmp props (:link/prompt link)])))))


(defn non-repeating-links [stage-tx! link navigate-cmp param-ctx]
  (->> (:link/link link)
       (remove :link/repeating?)
       (filter #(nil? (:link/field %)))
       (map (fn [link]
              (let [props (links/query-link stage-tx! link param-ctx)]
                ^{:key (:db/id link)}
                [navigate-cmp props (:link/prompt link)])))))


(defn pull-resultset [super-graph {find-elements :link/find-element :as link} create-new-find-elements resultset]
  (let [find-element-lookup (->> (mapv (juxt :find-element/name identity) find-elements)
                                 (into {}))]
    (->> (if (and (:link/single-result-as-entity? link) (= 0 (count resultset)))
           (let [local-result (->> find-elements
                                   (mapv (juxt :find-element/name #(get create-new-find-elements (:find-element/name %))))
                                   (into {}))]
             [local-result])
           resultset)
         (mapv (fn [result]
                 (->> result
                      (mapv (fn [[find-element-symbol entity-dbid]]
                              (let [connection (get-in find-element-lookup [find-element-symbol :find-element/connection :db/id :id])
                                    dbval (->DbVal connection nil)]
                                [find-element-symbol (hc/entity (hc/get-dbgraph super-graph dbval) entity-dbid)])))
                      (into {})))))))


(defn ui [stage-tx! super-graph
          {find-elements :link/find-element result-renderer-code :link/result-renderer :as link}
          {query-params :query-params create-new-find-elements :create-new-find-elements :as params-map}
          navigate-cmp param-ctx debug]
  (if-let [q (some-> link :link/query reader/read-string)]
    (let [params (merge (initial-params-map q query-params)
                        (q-util/build-dbhole-lookup link))
          param-ctx (assoc param-ctx :query-params query-params)]
      (if-not (holes-filled? (q-util/parse-holes q) params) ;todo what if we have a user hole?
        (if-not (:link/render-inline? link)                 ; don't show this error when we are nested
          [:div
           [:div "Unfilled query holes"]
           [:pre (doall (with-out-str
                          (binding [pprint/*print-miser-width* 1] ; not working
                            (pprint/pprint params))))]])
        (let [resultset (pull-resultset super-graph link create-new-find-elements
                                        (let [params (q-util/build-params (fn [hole-name param-ctx]
                                                                            (get params hole-name))
                                                                          link param-ctx)
                                              pull-exp (form/query-pull-exp find-elements)
                                              query-value [q params pull-exp]]
                                          (hc/select super-graph (hash query-value))))
              ordered-find-elements (find-elements-util/order-find-elements find-elements q)]
          (if (empty? result-renderer-code)
            (if (:link/single-result-as-entity? link)
              (let [result (first resultset)]
                [:div
                 (let [param-ctx (assoc param-ctx :result result)]
                   [:div
                    (map (fn [{:keys [:find-element/form] :as find-element}]
                           (let [entity (get result (:find-element/name find-element))]
                             ^{:key (hash [(.-dbid entity) (.-dbid form)])}
                             [form/form super-graph entity form (:link/link link) stage-tx! navigate-cmp param-ctx]))
                         ordered-find-elements)])
                 (->> (concat (repeating-links stage-tx! link result navigate-cmp param-ctx)
                              (non-repeating-links stage-tx! link navigate-cmp param-ctx))
                      (interpose " · "))])
              ^{:key (hc/t super-graph)}
              [table/table super-graph resultset ordered-find-elements (:link/link link) stage-tx! navigate-cmp param-ctx])
            (let [{result-renderer :value error :error} (eval result-renderer-code)
                  repeating-links (->> (:link/link link)
                                       (filter :link/repeating?)
                                       (mapv (juxt :link/ident identity))
                                       (into {}))
                  render-result (fn [result]
                                  (let [link-fn (fn [ident label]
                                                  (let [link (get repeating-links ident)
                                                        param-ctx (merge param-ctx {:result result})
                                                        props (links/query-link stage-tx! link param-ctx)]
                                                    [navigate-cmp props label]))]
                                    (try
                                      (result-renderer super-graph link-fn result)
                                      (catch :default e (pr-str e)))))]
              [:div
               (if error
                 [:pre (pprint/pprint error)]
                 (if (:link/single-result-as-entity? link)
                   (render-result (first resultset))
                   [:ul
                    (->> resultset
                         (map (fn [result]
                                [:li {:key (hash result)}
                                 (render-result result)])))]))
               [:div.links (interpose " · " (non-repeating-links stage-tx! link navigate-cmp param-ctx))]])))))
    [:div "Query record is incomplete"]))


(defn query [super-graph {find-elements :link/find-element :as link}
             {query-params :query-params create-new-find-elements :create-new-find-elements :as params-map}
             param-ctx debug]
  (if-let [q (some-> link :link/query reader/read-string)]
    (let [params (merge (initial-params-map q query-params)
                        (q-util/build-dbhole-lookup link))
          param-ctx (assoc param-ctx :query-params query-params)]
      (if (holes-filled? (q-util/parse-holes q) params)
        (let [p-filler (fn [link param-ctx]
                         (q-util/build-params #(get params %) link param-ctx))
              result-query [q
                            (p-filler link param-ctx)
                            (form/query-pull-exp find-elements)]
              inline-queries (let [inline-links (->> (:link/link link)
                                                     (filter :link/render-inline?))]
                               (some->> (hc/select super-graph (hash result-query))
                                        (pull-resultset super-graph link create-new-find-elements)
                                        (mapcat (fn [result]
                                                  (mapv (fn [inline-link]
                                                          (let [param-ctx (assoc param-ctx :result result
                                                                                           ;:query-params query-params
                                                                                           )
                                                                params-map (links/build-query-params inline-link param-ctx)
                                                                debug (str "inline-query:" (.-dbid inline-link))]
                                                            (query super-graph inline-link params-map param-ctx debug)))
                                                        inline-links)))
                                        (apply merge)))]
          (merge
            {(hash result-query) result-query}
            inline-queries
            (let [p-filler (if (:link/single-result-as-entity? link)
                             p-filler                       ; todo - why different?
                             q-util/build-params-from-formula)]
              (->> find-elements
                   (mapv (fn [{form :find-element/form :as find-element}]
                           (form/form-option-queries form p-filler param-ctx)))
                   (apply merge)))))))))
