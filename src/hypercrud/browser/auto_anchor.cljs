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
  "All sys links are :link/rel :sys, so they can be matched and merged with user-anchors.
  Matching is determined by [repeat? entity attribute ident]

  This function recurses in unexpected abstract way and impacts performance highly
  "
  [parent-fiddle ordered-fes ctx]
  (let [entity-links (->> ordered-fes
                          (filter :source-symbol)
                          (map-indexed (fn [fe-pos fe]
                                         (let [edit {:db/id {:ident :system-anchor-edit
                                                             :fe fe}
                                                     :hypercrud/sys? true
                                                     :anchor/prompt (str "edit-" (:name fe))
                                                     :link/rel (keyword (str "sys-edit-" (:name fe)))
                                                     :link/fiddle (auto-link/link-system-edit (:name fe))
                                                     :link/dependent? true
                                                     :link/managed? false
                                                     :link/path (str fe-pos)}
                                               ; create links mirror edit links but repeating false, see auto-formula.
                                               ; This is because the connection comes from the find-element, and when merging
                                               ; sys links we match on the find-element.
                                               new {:db/id {:ident :system-anchor-new
                                                            :fe fe}
                                                    :hypercrud/sys? true
                                                    :anchor/prompt (str "new-" (:name fe))
                                                    :link/rel (keyword (str "sys-new-" (:name fe)))
                                                    :link/fiddle (auto-link/link-system-edit (:name fe))
                                                    :link/dependent? false ; not managed, no parent-child ref
                                                    :link/path (str fe-pos)
                                                    :link/managed? true
                                                    :link/create? true
                                                    :link/render-inline? true}
                                               remove {:db/id {:ident :system-anchor-remove
                                                               :fe fe}
                                                       :hypercrud/sys? true
                                                       :anchor/prompt (str "remove-" (:name fe))
                                                       :link/rel (keyword (str "sys-remove-" (:name fe)))
                                                       :link/fiddle (auto-link/link-blank-system-remove (:name fe) nil)
                                                       :link/dependent? true
                                                       :link/path (str fe-pos)
                                                       :link/managed? true
                                                       :link/render-inline? true
                                                       :link/tx-fn (:entity-remove auto-anchor-txfn-lookup)}]
                                           (case (:fiddle/type parent-fiddle)
                                             :entity [remove]

                                             :query [edit new remove]

                                             :blank []))))
                          (apply concat)
                          doall)

        attr-links (if (not= :blank (:fiddle/type parent-fiddle))
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
                                                            :link/rel (keyword (str "sys-edit-" (:name fe) "-" attribute))
                                                            :link/dependent? true
                                                            :link/path (str fe-pos " " attribute)
                                                            :link/managed? false
                                                            :link/disabled? true
                                                            :link/fiddle (auto-link/link-system-edit-attr (:name fe) attribute)}
                                                           {:db/id {:ident :system-anchor-new-attr
                                                                    :fe fe
                                                                    :a attribute}
                                                            :hypercrud/sys? true
                                                            :anchor/prompt (str "new") ; conserve space in label
                                                            :link/rel (keyword (str "sys-new-" (:name fe) "-" attribute))
                                                            :link/dependent? true ; manged - need parent-child ref
                                                            :link/path (str fe-pos " " attribute)
                                                            :link/managed? true
                                                            :link/create? true
                                                            :link/render-inline? true
                                                            :link/disabled? true
                                                            :link/fiddle (auto-link/link-system-edit-attr (:name fe) attribute)}
                                                           {:db/id {:ident :system-anchor-remove-attr
                                                                    :fe fe
                                                                    :a attribute}
                                                            :hypercrud/sys? true
                                                            :anchor/prompt (str "remove")
                                                            :link/rel (keyword (str "sys-remove-" (:name fe) "-" attribute))
                                                            :link/fiddle (auto-link/link-blank-system-remove (:name fe) attribute)
                                                            :link/path (str fe-pos " " attribute)
                                                            :link/dependent? true
                                                            :link/managed? true
                                                            :link/render-inline? true
                                                            :link/disabled? true
                                                            :link/tx-fn (if (= :db.cardinality/one (get-in schema [attribute :db/cardinality :db/ident]))
                                                                            (:value-remove-one auto-anchor-txfn-lookup)
                                                                            (:value-remove-many auto-anchor-txfn-lookup))}]))))))
                          (apply concat)
                          doall))]
    (concat entity-links attr-links)))

(defn auto-anchor [link]
  (let [auto-fn (fn [link attr auto-f]
                  (let [v (get link attr)]
                    (if (or (not v) (and (string? v) (empty? v)))
                      (assoc link attr (auto-f link))
                      link)))]
    (-> link
        (auto-fn :link/tx-fn auto-txfn)
        (auto-fn :link/formula auto-formula))))

(defn merge-anchors [sys-links links]
  (->> (reduce (fn [grouped-links sys-link]
                 (update grouped-links
                         (:link/rel sys-link)
                         (fn [maybe-link-anchors]
                           (if maybe-link-anchors
                             (map (partial merge sys-link) maybe-link-anchors)
                             [sys-link]))))
               (->> links
                    (map #(into {} %))
                    (group-by #(or (:link/rel %) (:db/id %))))
               sys-links)
       vals
       flatten
       doall))

(defn auto-anchors [ordered-fes ctx]
  (let [sys-links (system-anchors (:fiddle ctx) ordered-fes ctx)
        links (->> (merge-anchors sys-links (get-in ctx [:fiddle :fiddle/links]))
                   (map auto-anchor))]
    (if (:keep-disabled-anchors? ctx)
      links
      (remove :link/disabled? links))))
