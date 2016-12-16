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


(defn holes-filled? [hole-names params-map]
  (set/subset? (set hole-names) (set (keys (into {} (remove (comp nil? val) params-map))))))


(defn repeating-links [link result {:keys [navigate-cmp] :as param-ctx}]
  (->> (:link/link link)
       (filter :link/repeating?)
       (filter #(nil? (:link/field %)))
       (mapv (fn [link]
               (let [param-ctx (merge param-ctx {:result result})
                     props (links/query-link link param-ctx)]
                 ^{:key (:db/id link)}
                 [navigate-cmp props (:link/prompt link)])))))


(defn non-repeating-links [link {:keys [navigate-cmp] :as param-ctx}]
  (->> (:link/link link)
       (remove :link/repeating?)
       (filter #(nil? (:link/field %)))
       (map (fn [link]
              (let [props (links/query-link link param-ctx)]
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


(defn ui [{find-elements :link/find-element result-renderer-code :link/result-renderer :as link}
          {query-params :query-params create-new-find-elements :create-new-find-elements :as params-map}
          {:keys [navigate-cmp super-graph] :as param-ctx}]
  (if-let [q (some-> link :link/query reader/read-string)]
    (let [params-map (merge query-params (q-util/build-dbhole-lookup link))
          param-ctx (assoc param-ctx :query-params query-params)
          query-hole-names (q-util/parse-holes q)]
      (if-not (holes-filled? query-hole-names params-map)   ;todo what if we have a user hole?
        (if-not (:link/render-inline? link)                 ; don't show this error when we are nested
          [:div
           [:div "Unfilled query holes"]
           [:pre (doall (with-out-str
                          (binding [pprint/*print-miser-width* 1] ; not working
                            (pprint/pprint (select-keys query-params query-hole-names)))))]])
        (let [resultset (pull-resultset super-graph link create-new-find-elements
                                        (let [params (q-util/build-params #(get params-map %) link param-ctx)
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
                             [form/form entity form (:link/link link) param-ctx]))
                         ordered-find-elements)])
                 (->> (concat (repeating-links link result param-ctx)
                              (non-repeating-links link param-ctx))
                      (interpose " · "))])
              ^{:key (hc/t super-graph)}
              [table/table resultset ordered-find-elements (:link/link link) param-ctx])
            (let [{result-renderer :value error :error} (eval result-renderer-code)
                  repeating-links (->> (:link/link link)
                                       (filter :link/repeating?)
                                       (mapv (juxt :link/ident identity))
                                       (into {}))
                  render-result (fn [result]
                                  (let [link-fn (fn [ident label]
                                                  (let [link (get repeating-links ident)
                                                        param-ctx (merge param-ctx {:result result})
                                                        props (links/query-link link param-ctx)]
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
               [:div.links (interpose " · " (non-repeating-links link param-ctx))]])))))
    [:div "Query record is incomplete"]))


(declare query)


(defn field-queries [param-ctx field]
  (let [{:keys [:attribute/valueType :attribute/isComponent]} (:field/attribute field)
        is-ref (= (:db/ident valueType) :db.type/ref)]
    ; if we are a ref we ALWAYS need the query from the field options
    ; EXCEPT when we are component, in which case no options are rendered, just a form, handled below
    (if (and is-ref (not isComponent))
      (if-let [options-link (:field/options-link field)]
        (let [params-map (links/build-params-map options-link param-ctx)
              param-ctx (assoc param-ctx :debug (str "field-options:" (:db/id field)))]
          (query options-link params-map param-ctx))))))


(defn form-option-queries "get the form options recursively for all expanded forms"
  [form param-ctx]
  (apply merge
         (mapv #(field-queries param-ctx %) (:form/field form))))


(defn dependent-queries [{find-elements :link/find-element :as link} resultset param-ctx]
  (let [inline-links (->> (:link/link link)
                          (filter :link/render-inline?))]
    (->> resultset
         (mapcat (fn [result]
                   (let [param-ctx (assoc param-ctx :result result)
                         option-queries (mapv (fn [{form :find-element/form :as find-element}]
                                                (form-option-queries form param-ctx))
                                              find-elements)
                         inline-queries (mapv (fn [inline-link]
                                                (let [params-map (links/build-params-map inline-link param-ctx)
                                                      param-ctx (assoc param-ctx :debug (str "inline-query:" (.-dbid inline-link)))]
                                                  (query inline-link params-map param-ctx)))
                                              inline-links)]
                     (concat option-queries inline-queries))))
         (apply merge))))


(defn query [{find-elements :link/find-element :as link}
             {query-params :query-params create-new-find-elements :create-new-find-elements :as params-map}
             {:keys [super-graph] :as param-ctx}]
  (if-let [q (some-> link :link/query reader/read-string)]
    (let [params-map (merge query-params (q-util/build-dbhole-lookup link))
          param-ctx (assoc param-ctx :query-params query-params)]
      (if (holes-filled? (q-util/parse-holes q) params-map)
        (let [result-query [q
                            (q-util/build-params #(get params-map %) link param-ctx)
                            (form/query-pull-exp find-elements)]]
          (merge
            {(hash result-query) result-query}
            (if-let [resultset (some->> (hc/select super-graph (hash result-query))
                                        (pull-resultset super-graph link create-new-find-elements))]
              (dependent-queries link resultset param-ctx))))))))
