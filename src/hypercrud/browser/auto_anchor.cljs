(ns hypercrud.browser.auto-anchor
  (:require-macros [hypercrud.util.template :as template])
  (:require [hypercrud.browser.auto-anchor-formula :refer [auto-formula]]
            [hypercrud.browser.auto-anchor-txfn :refer [auto-txfn]]
            [hypercrud.browser.auto-link :as auto-link]
            [hypercrud.client.core :as hc]
            [hypercrud.types.DbId :refer [->DbId]]
            [hypercrud.util.vedn :as vedn]))


(defn system-anchor? [link-dbid]
  (map? (:id link-dbid)))

(def auto-anchor-txfn-lookup
  (->> (template/load-resource "auto-anchor/tx-fns.vedn")
       (vedn/read-string)))

(defn system-anchors
  "All sys links are :anchor/ident :sys, so they can be matched and merged with user-anchors.
  Matching is determined by [repeat? entity attribute ident]

  This function recurses in unexpected abstract way and impacts performance highly
  "
  [parent-link ordered-fes param-ctx]
  (let [entity-links (->> ordered-fes
                          (mapcat (fn [{fe-name :find-element/name fe-conn :find-element/connection :as fe}]
                                    (let [edit {:db/id (->DbId {:ident :system-anchor-edit
                                                                :fe (-> fe :db/id :id)}
                                                               hc/*root-conn-id*)
                                                :anchor/prompt (str "edit-" fe-name)
                                                :anchor/ident (keyword (str "sys-edit-" fe-name))
                                                :anchor/link (auto-link/link-system-edit fe-name fe-conn)
                                                :anchor/repeating? true
                                                :anchor/managed? false
                                                :anchor/find-element fe}
                                          ; create links mirror edit links but repeating false, see auto-formula.
                                          ; This is because the connection comes from the find-element, and when merging
                                          ; sys links we match on the find-element.
                                          new {:db/id (->DbId {:ident :system-anchor-new
                                                               :fe (-> fe :db/id :id)}
                                                              hc/*root-conn-id*)
                                               :anchor/prompt (str "new-" fe-name)
                                               :anchor/ident (keyword (str "sys-new-" fe-name))
                                               :anchor/link (auto-link/link-system-edit fe-name fe-conn)
                                               :anchor/repeating? false ; not managed, no parent-child ref
                                               :anchor/find-element fe
                                               :anchor/managed? true
                                               :anchor/create? true
                                               :anchor/render-inline? true}
                                          remove {:db/id (->DbId {:ident :system-anchor-remove
                                                                  :fe (-> fe :db/id :id)}
                                                                 hc/*root-conn-id*)
                                                  :anchor/prompt (str "remove-" fe-name)
                                                  :anchor/ident (keyword (str "sys-remove-" fe-name))
                                                  :anchor/link (auto-link/link-blank-system-remove fe-name nil)
                                                  :anchor/repeating? true
                                                  :anchor/find-element fe
                                                  :anchor/managed? true
                                                  :anchor/render-inline? true
                                                  :anchor/tx-fn (:entity-remove auto-anchor-txfn-lookup)}]
                                      (case (:request/type parent-link)
                                        :entity [remove]

                                        :query [edit new remove]

                                        :blank []))))
                          doall)

        attr-links (if (not= :blank (:request/type parent-link))
                     (->> ordered-fes
                          (mapcat (fn [{fe-name :find-element/name fe-conn :find-element/connection :as fe}]
                                    (let [schema (get-in param-ctx [:schemas fe-name])]
                                      (->> (-> fe :find-element/form :form/field)
                                           (filter (fn [{:keys [:field/attribute]}]
                                                     (and (not= attribute :db/id)
                                                          (= :db.type/ref (get-in schema [attribute :db/valueType :db/ident])))))
                                           (mapcat (fn [{:keys [:field/attribute]}]
                                                     [{:db/id (->DbId {:ident :system-anchor-edit-attr
                                                                       :fe (-> fe :db/id :id)
                                                                       :a attribute}
                                                                      hc/*root-conn-id*)
                                                       :anchor/prompt (str "edit") ; conserve space in label
                                                       :anchor/ident (keyword (str "sys-edit-" fe-name "-" attribute))
                                                       :anchor/repeating? true
                                                       :anchor/find-element fe
                                                       :anchor/attribute attribute
                                                       :anchor/managed? false
                                                       :anchor/disabled? true
                                                       :anchor/link (auto-link/link-system-edit-attr fe-name fe-conn attribute)}
                                                      {:db/id (->DbId {:ident :system-anchor-new-attr
                                                                       :fe (-> fe :db/id :id)
                                                                       :a attribute}
                                                                      hc/*root-conn-id*)
                                                       :anchor/prompt (str "new") ; conserve space in label
                                                       :anchor/ident (keyword (str "sys-new-" fe-name "-" attribute))
                                                       :anchor/repeating? true ; manged - need parent-child ref
                                                       :anchor/find-element fe
                                                       :anchor/attribute attribute
                                                       :anchor/managed? true
                                                       :anchor/create? true
                                                       :anchor/render-inline? true
                                                       :anchor/disabled? true
                                                       :anchor/link (auto-link/link-system-edit-attr fe-name fe-conn attribute)}
                                                      {:db/id (->DbId {:ident :system-anchor-remove-attr
                                                                       :fe (-> fe :db/id :id)
                                                                       :a attribute}
                                                                      hc/*root-conn-id*)
                                                       :anchor/prompt (str "remove")
                                                       :anchor/ident (keyword (str "sys-remove-" fe-name "-" attribute))
                                                       :anchor/link (auto-link/link-blank-system-remove fe-name attribute)
                                                       :anchor/find-element fe
                                                       :anchor/attribute attribute
                                                       :anchor/repeating? true
                                                       :anchor/managed? true
                                                       :anchor/render-inline? true
                                                       :anchor/disabled? true
                                                       :anchor/tx-fn (if (= :db.cardinality/one (get-in schema [attribute :db/cardinality :db/ident]))
                                                                       (:value-remove-one auto-anchor-txfn-lookup)
                                                                       (:value-remove-many auto-anchor-txfn-lookup))}]))))))
                          doall))]
    (concat entity-links attr-links)))

(defn auto-anchor [anchor]
  (let [auto-fn (fn [anchor attr auto-f]
                  (let [v (get anchor attr)]
                    (if (or (not v) (and (string? v) (empty? v)))
                      (assoc anchor attr (auto-f anchor))
                      anchor)))]
    (-> anchor
        (auto-fn :anchor/tx-fn auto-txfn)
        (auto-fn :anchor/formula auto-formula))))

(defn merge-anchors [sys-anchors link-anchors]
  (->> (reduce (fn [grouped-link-anchors sys-anchor]
                 (update grouped-link-anchors
                         (:anchor/ident sys-anchor)
                         (fn [maybe-link-anchors]
                           (if maybe-link-anchors
                             (map (partial merge sys-anchor) maybe-link-anchors)
                             [sys-anchor]))))
               (group-by #(or (:anchor/ident %) (:db/id %)) link-anchors)
               sys-anchors)
       vals
       flatten
       doall))

(defn auto-anchors [link ordered-fes param-ctx & [{:keys [ignore-user-links]}]]
  (let [sys-anchors (system-anchors link ordered-fes param-ctx)]
    (->> (if ignore-user-links
           sys-anchors
           (merge-anchors sys-anchors (:link/anchor link)))
         (map auto-anchor))))
