(ns hypercrud.browser.pages.query
  (:require [cljs.reader :as reader]
            [clojure.set :as set]
            [hypercrud.browser.links :as links]
            [hypercrud.browser.pages.entity :as entity]
            [hypercrud.client.core :as hc]
            [hypercrud.client.tx :as tx-util]
            [hypercrud.compile.eval :as eval]
            [hypercrud.form.q-util :as q-util]
            [hypercrud.form.util :as form-util]
            [hypercrud.types :refer [->DbId ->DbVal ->Entity]]
            [hypercrud.ui.auto-control :refer [auto-control]]
            [hypercrud.ui.form :as form]
            [hypercrud.ui.table :as table]
            [hypercrud.client.graph :as hc-g]
            [hypercrud.client.tx :as tx]))


(defn initial-entity [q holes-by-name params]
  (let [schema (->> holes-by-name
                    (map (fn [[hole-name hole]]
                           [hole-name {:db/cardinality (:attribute/cardinality hole)}]))
                    (into {}))
        hole-names (q-util/parse-holes q)
        dbid (->DbId -1 :query-hole-form)
        dbval (->DbVal :query-hole-form nil)
        data (-> (zipmap hole-names params)
                 (assoc :db/id dbid))]
    (->Entity (hc-g/->DbGraph schema dbval nil nil) dbid data)))


(defn show-results? [hole-names param-values]
  (set/subset? (set hole-names) (set (keys (into {} (remove (comp nil? val) param-values))))))


;; Why is this a hc/with - a fake entity?
;; Because we're faking a dynamic formfield entity, which needs to be attached to the real editor graph
(defn holes->field-tx [editor-graph form-dbid hole-names holes-by-name]
  ;todo these ids are scary
  (->> hole-names
       (map #(get holes-by-name %))
       (mapcat (fn [{:keys [:hole/name :hole/formula
                            :field/prompt :field/query :field/label-prop
                            :attribute/valueType :attribute/cardinality] :as hole}]
                 (let [field-dbid (->DbId (+ 5555555 (-> hole :db/id .-id)) (-> editor-graph .-dbval .-conn-id))
                       attr-dbid (->DbId (+ 6555555 (-> hole :db/id .-id)) (-> editor-graph .-dbval .-conn-id))]
                   [[:db/add attr-dbid :attribute/ident name]
                    [:db/add attr-dbid :attribute/valueType (.-dbid valueType)]
                    [:db/add attr-dbid :attribute/cardinality (.-dbid cardinality)]
                    [:db/add field-dbid :field/prompt prompt]
                    [:db/add field-dbid :field/query query]
                    [:db/add field-dbid :field/label-prop label-prop]
                    [:db/add field-dbid :field/attribute attr-dbid]
                    [:db/add field-dbid :link/formula formula]
                    [:db/add form-dbid :form/field field-dbid]])))))


(defn ui [cur editor-graph transact! graph table-form query params navigate! navigate-cmp param-ctx]
  (let [q (reader/read-string (:query/value query))
        hole-names (q-util/parse-holes q)
        local-statements (cur [:statements] [])
        expanded-cur (cur [:expanded] {})
        holes-by-name (->> (:query/hole query)
                           (map (juxt :hole/name identity))
                           (into {}))
        entity-cur (cur [:holes] (initial-entity q hole-names params)) ;todo this needs to be hc/with
        entity @entity-cur
        dbval (get param-ctx :dbval)
        graph (hc/with graph @local-statements)]
    [:div
     #_[:pre (pr-str params)]                               ;; params are same information as the filled holes in this form below
     (let [stage-tx! (fn [tx]
                       (reset! entity-cur (reduce tx/merge-entity-and-stmt entity tx)))
           holes-form-dbid (->DbId -1 (-> editor-graph .-dbval .-conn-id))
           editor-graph (hc/with' editor-graph (holes->field-tx editor-graph holes-form-dbid hole-names holes-by-name))
           holes-form (hc/entity editor-graph holes-form-dbid)]
       [form/form graph entity holes-form expanded-cur stage-tx! navigate-cmp])
     [:hr]
     (if (show-results? hole-names (-> entity .-data (dissoc :db/id))) ;todo what if we have a user hole?
       (let [entities (->> (hc/select graph ::table/query)
                           (map #(hc/entity (hc/get-dbgraph graph dbval) %)))]
         (if (and (:query/single-result-as-entity? query) (= 1 (count entities)))
           [entity/ui cur transact! graph (first entities) table-form navigate! navigate-cmp]
           [:div
            (let [row-renderer-code (:form/row-renderer table-form)
                  stage-tx! #(swap! local-statements tx-util/into-tx %)]
              (if (empty? row-renderer-code)
                [table/table graph entities table-form nil expanded-cur stage-tx! navigate-cmp nil]
                (let [result (eval/uate (str "(identity " row-renderer-code ")"))
                      {row-renderer :value error :error} result]
                  (if error
                    [:div (pr-str error)]
                    [:div
                     [:ul
                      (->> entities
                           (map (fn [entity]
                                  (let [link-fn (fn [ident label]
                                                  (let [link (->> (:form/link table-form)
                                                                  (filter #(= ident (:link/ident %)))
                                                                  first)
                                                        param-ctx (merge param-ctx {:entity entity})
                                                        href (links/query-link link param-ctx)]
                                                    [navigate-cmp {:href href} label]))]
                                    [:li {:key (hash (:db/id entity))}
                                     (try
                                       (row-renderer graph link-fn entity)
                                       (catch :default e (pr-str e)))]))))]]))))
            [:button {:key 1 :on-click #(transact! @local-statements)} "Save"]])))]))


(defn query [state editor-graph query params table-form param-ctx]
  (let [q (reader/read-string (:query/value query))
        hole-names (q-util/parse-holes q)
        ;; this is the new "param-ctx" - it is different - we already have the overridden
        ;; values, there is no need to eval the formulas in a ctx to get the values.
        param-values (-> (or (get state :holes)
                             (initial-entity q hole-names params))
                         .-data
                         (dissoc :db/id))]
    (merge
      ;no fields are isComponent true or expanded, so we don't need to pass in a forms map
      (let [holes-by-name (->> (:query/hole query)
                               (map (juxt :hole/name identity))
                               (into {}))
            holes-form-dbid (->DbId -1 (-> editor-graph .-dbval .-conn-id))
            editor-graph (hc/with' editor-graph (holes->field-tx editor-graph holes-form-dbid hole-names holes-by-name))
            holes-form (hc/entity editor-graph holes-form-dbid)]
        (form-util/form-option-queries holes-form nil q-util/build-params-from-formula param-ctx))

      (if (show-results? hole-names param-values)
        (let [p-filler (fn [query formulas param-ctx]
                         (q-util/build-params #(get param-values %) query param-ctx))]
          ; hacks so when result set is 1 we can display the entity view
          #_(table/query p-filler param-ctx {:link/query (:db/id query)
                                             :link/form form-id})
          (let [expanded-forms (get state :expanded nil)
                dbval (get param-ctx :dbval)]
            (if (:query/single-result-as-entity? query)
              ; we can use nil for :link/formula and formulas because we know our p-filler doesn't use it
              (merge
                (form-util/form-option-queries table-form expanded-forms p-filler param-ctx)
                (table/option-queries table-form p-filler param-ctx)
                {:hypercrud.ui.table/query [q (p-filler query nil param-ctx) [dbval (form-util/form-pull-exp table-form expanded-forms)]]})
              (table/query p-filler param-ctx query table-form nil))))))))
