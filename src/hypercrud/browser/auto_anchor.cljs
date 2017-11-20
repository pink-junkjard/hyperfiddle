(ns hypercrud.browser.auto-anchor
  (:require-macros [hypercrud.util.template :as template])
  (:require [hypercrud.browser.auto-anchor-formula :refer [auto-formula]]
            [hypercrud.browser.auto-anchor-txfn :refer [auto-txfn]]
            [hypercrud.browser.auto-link :as auto-link]
            [hypercrud.util.vedn :as vedn]))


(defn system-anchor? [link-id]
  (map? link-id))

(def auto-anchor-txfn-lookup
  (->> (template/load-resource "auto-anchor/tx-fns.vedn")
       (vedn/read-string)))

(defn system-anchors
  "All sys links are :anchor/ident :sys, so they can be matched and merged with user-anchors.
  Matching is determined by [repeat? entity attribute ident]

  This function recurses in unexpected abstract way and impacts performance highly
  "
  [parent-link ordered-fes ctx]
  (let [entity-links (->> ordered-fes
                          (filter :source-symbol)
                          (map-indexed (fn [fe-pos fe]
                                         (let [edit {:db/id {:ident :system-anchor-edit
                                                             :fe fe}
                                                     :hypercrud/sys? true
                                                     :anchor/prompt (str "edit-" (:name fe))
                                                     :anchor/ident (keyword (str "sys-edit-" (:name fe)))
                                                     :anchor/link (auto-link/link-system-edit (:name fe))
                                                     :anchor/repeating? true
                                                     :anchor/managed? false
                                                     :link/path (str fe-pos)}
                                               ; create links mirror edit links but repeating false, see auto-formula.
                                               ; This is because the connection comes from the find-element, and when merging
                                               ; sys links we match on the find-element.
                                               new {:db/id {:ident :system-anchor-new
                                                            :fe fe}
                                                    :hypercrud/sys? true
                                                    :anchor/prompt (str "new-" (:name fe))
                                                    :anchor/ident (keyword (str "sys-new-" (:name fe)))
                                                    :anchor/link (auto-link/link-system-edit (:name fe))
                                                    :anchor/repeating? false ; not managed, no parent-child ref
                                                    :link/path (str fe-pos)
                                                    :anchor/managed? true
                                                    :anchor/create? true
                                                    :anchor/render-inline? true}
                                               remove {:db/id {:ident :system-anchor-remove
                                                               :fe fe}
                                                       :hypercrud/sys? true
                                                       :anchor/prompt (str "remove-" (:name fe))
                                                       :anchor/ident (keyword (str "sys-remove-" (:name fe)))
                                                       :anchor/link (auto-link/link-blank-system-remove (:name fe) nil)
                                                       :anchor/repeating? true
                                                       :link/path (str fe-pos)
                                                       :anchor/managed? true
                                                       :anchor/render-inline? true
                                                       :anchor/tx-fn (:entity-remove auto-anchor-txfn-lookup)}]
                                           (case (:request/type parent-link)
                                             :entity [remove]

                                             :query [edit new remove]

                                             :blank []))))
                          (apply concat)
                          doall)

        attr-links (if (not= :blank (:request/type parent-link))
                     (->> ordered-fes
                          (filter :source-symbol)
                          (map-indexed (fn [fe-pos fe]
                                         (let [schema (get-in ctx [:schemas (str (:source-symbol fe))])]
                                           (->> (:fields fe)
                                                (filter (fn [{:keys [attribute]}]
                                                          (and (not= attribute :db/id)
                                                               (= :db.type/ref (get-in schema [attribute :db/valueType :db/ident])))))
                                                (mapcat (fn [{:keys [attribute]}]
                                                          [{:db/id {:ident :system-anchor-edit-attr
                                                                    :fe fe
                                                                    :a attribute}
                                                            :hypercrud/sys? true
                                                            :anchor/prompt (str "edit") ; conserve space in label
                                                            :anchor/ident (keyword (str "sys-edit-" (:name fe) "-" attribute))
                                                            :anchor/repeating? true
                                                            :link/path (str fe-pos " " attribute)
                                                            :anchor/managed? false
                                                            :anchor/disabled? true
                                                            :anchor/link (auto-link/link-system-edit-attr (:name fe) attribute)}
                                                           {:db/id {:ident :system-anchor-new-attr
                                                                    :fe fe
                                                                    :a attribute}
                                                            :hypercrud/sys? true
                                                            :anchor/prompt (str "new") ; conserve space in label
                                                            :anchor/ident (keyword (str "sys-new-" (:name fe) "-" attribute))
                                                            :anchor/repeating? true ; manged - need parent-child ref
                                                            :link/path (str fe-pos " " attribute)
                                                            :anchor/managed? true
                                                            :anchor/create? true
                                                            :anchor/render-inline? true
                                                            :anchor/disabled? true
                                                            :anchor/link (auto-link/link-system-edit-attr (:name fe) attribute)}
                                                           {:db/id {:ident :system-anchor-remove-attr
                                                                    :fe fe
                                                                    :a attribute}
                                                            :hypercrud/sys? true
                                                            :anchor/prompt (str "remove")
                                                            :anchor/ident (keyword (str "sys-remove-" (:name fe) "-" attribute))
                                                            :anchor/link (auto-link/link-blank-system-remove (:name fe) attribute)
                                                            :link/path (str fe-pos " " attribute)
                                                            :anchor/repeating? true
                                                            :anchor/managed? true
                                                            :anchor/render-inline? true
                                                            :anchor/disabled? true
                                                            :anchor/tx-fn (if (= :db.cardinality/one (get-in schema [attribute :db/cardinality :db/ident]))
                                                                            (:value-remove-one auto-anchor-txfn-lookup)
                                                                            (:value-remove-many auto-anchor-txfn-lookup))}]))))))
                          (apply concat)
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
        (auto-fn :link/formula auto-formula))))

(defn merge-anchors [sys-anchors link-anchors]
  (->> (reduce (fn [grouped-link-anchors sys-anchor]
                 (update grouped-link-anchors
                         (:anchor/ident sys-anchor)
                         (fn [maybe-link-anchors]
                           (if maybe-link-anchors
                             (map (partial merge sys-anchor) maybe-link-anchors)
                             [sys-anchor]))))
               (->> link-anchors
                    (map #(into {} %))
                    (group-by #(or (:anchor/ident %) (:db/id %))))
               sys-anchors)
       vals
       flatten
       doall))

(defn auto-anchors [ordered-fes ctx]
  (let [sys-anchors (system-anchors (:fiddle ctx) ordered-fes ctx)
        anchors (->> (merge-anchors sys-anchors (get-in ctx [:fiddle :link/anchor]))
                     (map auto-anchor))]
    (if (:keep-disabled-anchors? ctx)
      anchors
      (remove :anchor/disabled? anchors))))
