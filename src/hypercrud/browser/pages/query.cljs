(ns hypercrud.browser.pages.query
  (:require [clojure.set :as set]
            [hypercrud.browser.links :as links]
            [hypercrud.browser.pages.entity :as entity]
            [hypercrud.client.core :as hc]
            [hypercrud.client.tx :as tx-util]
            [hypercrud.compile.eval :as eval]
            [hypercrud.form.field :as field]
            [hypercrud.form.q-util :as q-util]
            [hypercrud.form.util :as form-util]
            [hypercrud.types :as types]
            [hypercrud.ui.auto-control :refer [auto-control]]
            [hypercrud.ui.form :as form]
            [hypercrud.ui.table :as table]))


(defn initial-entity [q params]
  (let [hole-names (q-util/parse-holes q)]
    (-> (zipmap hole-names params)                          ;todo touch up how params are sent over the wire
        (assoc :db/id -1))))


(defn show-results? [hole-names param-values]
  (set/subset? (set hole-names) (set (keys (into {} (remove (comp nil? val) param-values))))))


;get query metadata from query edn
(defn get-query [queries q]
  (->> (vals queries)
       (filter #(= q (:query/value %)))
       first))


(defn ui [cur transact! graph forms queries form-id {:keys [q params]} navigate! navigate-cmp param-ctx]
  (let [query (get-query queries q)
        hole-names (q-util/parse-holes (:query/value query))
        local-statements (cur [:statements] [])
        expanded-cur (cur [:expanded] {})
        holes-by-name (->> (:query/hole query)
                           (map (juxt :hole/name identity))
                           (into {}))
        table-form (get forms form-id)
        entity-cur (cur [:holes] (initial-entity q params))
        entity @entity-cur
        dbval (get param-ctx :dbval)
        graph (hc/with graph dbval @local-statements)]
    [:div
     [:pre (pr-str q)]
     [:pre (pr-str params)]
     (let [schema (->> holes-by-name
                       (map (fn [[hole-name hole]]
                              [hole-name {:db/cardinality (:attribute/cardinality hole)}]))
                       (into {}))
           stage-tx! (fn [tx]
                       (reset! entity-cur (reduce
                                            #(tx-util/apply-stmt-to-entity schema %1 %2)
                                            entity
                                            tx)))
           holes-form {:db/id -1
                       :form/field (map #(field/hole->field (get holes-by-name %)) hole-names)}]
       [form/form2 graph entity (assoc forms -1 holes-form) queries holes-form expanded-cur stage-tx! navigate-cmp])
     [:hr]
     (if (show-results? hole-names entity)                  ;todo what if we have a user hole?
       (let [results (hc/select graph ::table/query)]
         (if (and (:query/single-result-as-entity? query) (= 1 (count results)))
           (let [dbval (:dbval param-ctx)
                 dbid (types/->DbId (first results) (.-conn-id dbval))]
             [entity/ui cur transact! graph dbval dbid forms queries form-id navigate! navigate-cmp])
           [:div
            (let [row-renderer-code (:form/row-renderer table-form)
                  stage-tx! #(swap! local-statements tx-util/into-tx %)]
              (if (empty? row-renderer-code)
                [table/table graph (:dbval param-ctx) results forms queries form-id expanded-cur stage-tx! navigate-cmp nil param-ctx]
                (let [result (eval/uate (str "(identity " row-renderer-code ")"))
                      {row-renderer :value error :error} result]
                  (if error
                    [:div (pr-str error)]
                    [:div
                     [:ul
                      (->> results
                           (map #(hc/entity graph dbval %))
                           (map (fn [entity]
                                  (let [link-fn (fn [ident label]
                                                  (let [link (->> (:form/link table-form)
                                                                  (filter #(= ident (:link/ident %)))
                                                                  first)
                                                        param-ctx (merge param-ctx {:entity entity})
                                                        href (links/query-link link queries param-ctx)]
                                                    [navigate-cmp {:href href} label]))]
                                    [:li {:key (hash (:db/id entity))}
                                     (try
                                       (row-renderer graph link-fn entity)
                                       (catch :default e (pr-str e)))]))))]]))))
            [:button {:key 1 :on-click #(transact! @local-statements)} "Save"]])))]))


(defn query [state forms queries {:keys [q params]} form-id param-ctx]
  (let [query (get-query queries q)
        hole-names (q-util/parse-holes q)
        holes-form {:db/id -1
                    :form/field (vec (map field/hole->field (:query/hole query)))}
        table-form (get forms form-id)

        ;; this is the new "param-ctx" - it is different - we already have the overridden
        ;; values, there is no need to eval the formulas in a ctx to get the values.
        param-values (-> (or (get state :holes)
                             (initial-entity q params))
                         (dissoc :db/id))]
    (merge
      ;no fields are isComponent true or expanded, so we don't need to pass in a forms map
      (form-util/form-option-queries nil queries holes-form nil q-util/build-params-from-formula param-ctx)

      (if (show-results? hole-names param-values)
        (let [p-filler (fn [query formulas param-ctx]
                         (q-util/build-params #(get param-values %) query param-ctx))]
          ; hacks so when result set is 1 we can display the entity view
          #_(table/query forms queries p-filler param-ctx {:link/query (:db/id query)
                                                           :link/form form-id})
          (let [expanded-forms (get state :expanded nil)
                dbval (get param-ctx :dbval)]
            (if (:query/single-result-as-entity? query)
              ; we can use nil for :link/formula and formulas because we know our p-filler doesn't use it
              (merge
                (form-util/form-option-queries forms queries table-form expanded-forms p-filler param-ctx)
                (table/option-queries queries table-form p-filler param-ctx)
                {:hypercrud.ui.table/query [(:query/value query) (p-filler query nil param-ctx) [dbval (form-util/form-pull-exp forms table-form expanded-forms)]]})
              (table/query forms queries p-filler param-ctx {:link/query (:db/id query)
                                                             :link/form form-id}))))))))
