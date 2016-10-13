(ns hypercrud.browser.pages.query
  (:require [cljs.pprint :as pprint]
            [clojure.set :as set]
            [hypercrud.browser.links :as links]
            [hypercrud.browser.pages.entity :as entity]
            [hypercrud.client.core :as hc]
            [hypercrud.client.tx :as tx-util]
            [hypercrud.compile.eval :as eval]
            [hypercrud.form.field :as field]
            [hypercrud.form.q-util :as q-util]
            [hypercrud.form.util :as form-util]
            [hypercrud.ui.auto-control :refer [auto-control]]
            [hypercrud.ui.form :as form]
            [hypercrud.ui.table :as table]))


(defn initial-entity [q params]
  (-> (zipmap (q-util/parse-holes q) params)
      (assoc :db/id -1)))


(defn show-results? [hole-names param-values]
  (set/subset? (set hole-names) (set (keys (into {} (remove (comp nil? val) param-values))))))


;get query metadata from query edn
(defn get-query [queries q]
  (->> (vals queries)
       (filter #(= q (:query/value %)))
       first))


(defn ui [cur transact! graph forms queries form-id {:keys [q params]} navigate! navigate-cmp]
  (let [query (get-query queries q)
        hole-names (q-util/parse-holes (:query/value query))
        local-statements (cur [:statements] [])
        expanded-cur (cur [:expanded] {})
        holes-by-name (->> (:query/hole query)
                           (map (juxt :hole/name identity))
                           (into {}))
        entity-cur (cur [:holes] (initial-entity q params))
        entity @entity-cur
        graph (hc/with graph @local-statements)]
    [:div
     [:pre (with-out-str (pprint/pprint [q params]))]
     (let [schema (->> holes-by-name
                       (map (fn [[hole-name hole]]
                              [hole-name {:db/cardinality (:attribute/cardinality hole)}]))
                       (into {}))
           stage-tx! (fn [tx]
                       (reset! entity-cur (reduce
                                            #(tx-util/apply-stmt-to-entity schema %1 %2)
                                            entity
                                            tx)))
           form {:form/field (map #(field/hole->field (get holes-by-name %)) hole-names)}]
       [form/form2 graph entity forms queries form expanded-cur stage-tx! navigate-cmp])
     [:hr]
     (if (show-results? hole-names entity)                  ;todo what if we have a user hole?
       (let [results (hc/select graph ::table/query)]
         (if (= 1 (count results))
           [entity/ui cur transact! graph (first results) forms queries form-id navigate! navigate-cmp]
           (let [form (get forms (:query/form query))
                 row-renderer-code (:form/row-renderer form)]
             (if (empty? row-renderer-code)
               (let [stage-tx! #(swap! local-statements tx-util/into-tx %)]
                 [table/table graph results forms queries form-id expanded-cur stage-tx! navigate-cmp nil])
               (let [result (eval/uate (str "(identity " row-renderer-code ")"))
                     {row-renderer :value error :error} result]
                 (if error
                   [:div (pr-str error)]
                   [:ul
                    (->> results
                         (map #(hc/entity graph %))
                         (map (fn [entity]
                                (let [link-fn (fn [ident label]
                                                (let [query (->> (:form/link form)
                                                                 (filter #(= ident (:link/ident %)))
                                                                 first
                                                                 :link/query
                                                                 (get queries))
                                                      param-ctx {:entity entity}
                                                      href (links/query-link query param-ctx)]
                                                  [navigate-cmp {:href href} label]))]
                                  [:li {:key (:db/id entity)}
                                   (try
                                     (row-renderer graph link-fn entity)
                                     (catch :default e (pr-str e)))]))))])))))))
     [:button {:key 1 :on-click #(transact! @local-statements)} "Save"]]))


(defn query [state forms queries {:keys [q params]} form-id param-ctx]
  (let [query (get-query queries q)
        hole-names (q-util/parse-holes q)
        form {:form/field (vec (map field/hole->field (:query/hole query)))}

        ;; this is the new "param-ctx" - it is different - we already have the overridden
        ;; values, there is no need to eval the formulas in a ctx to get the values.
        param-values (-> (or (get state :holes)
                             (initial-entity q params))
                         (dissoc :db/id))]
    (merge
      ;no fields are isComponent true or expanded, so we don't need to pass in a forms map
      (form-util/form-option-queries nil queries form nil q-util/build-params-from-formula param-ctx)

      (if (show-results? hole-names param-values)
        (let [p-filler (partial q-util/build-params #(get param-values %))]
          (table/query query p-filler param-ctx forms queries form-id))))))
