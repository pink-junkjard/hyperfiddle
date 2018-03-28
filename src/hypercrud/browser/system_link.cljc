(ns hypercrud.browser.system-link                           ; this namespace is public extern
  (:require [hypercrud.browser.system-fiddle :as system-fiddle]
            [hypercrud.util.vedn :as vedn]))


(defn ^:export system-link? [link-id]
  (map? link-id))

(def txfn-lookup (vedn/load-vedn-from-file "system-link/tx-fns.vedn"))

(defn system-links
  "All sys links are :link/rel :sys, so they can be matched and merged with user-links.
  Matching is determined by [repeat? entity attribute ident]

  This appfn recurses in unexpected abstract way and impacts performance highly
  "
  [parent-fiddle ordered-fes schemas]
  (let [entity-links (->> ordered-fes
                          (map-indexed (fn [fe-pos fe]
                                         (when (:source-symbol fe)
                                           (let [edit {:db/id :system-anchor-edit
                                                       :hypercrud/sys? true
                                                       :anchor/prompt (str "edit-" (:name fe))
                                                       :link/rel (keyword (str "sys-edit-" (:name fe)))
                                                       :link/fiddle system-fiddle/fiddle-system-edit
                                                       :link/dependent? true
                                                       :link/managed? false
                                                       :link/path (str fe-pos)}
                                                 ; create links mirror edit links but repeating false, see auto-formula.
                                                 ; This is because the connection comes from the find-element, and when merging
                                                 ; sys links we match on the find-element.
                                                 new {:db/id :system-anchor-new
                                                      :hypercrud/sys? true
                                                      :anchor/prompt (str "new-" (:name fe))
                                                      :link/rel (keyword (str "sys-new-" (:name fe)))
                                                      :link/fiddle system-fiddle/fiddle-system-edit
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
                                                         :link/fiddle (system-fiddle/fiddle-blank-system-remove)
                                                         :link/dependent? true
                                                         :link/path (str fe-pos)
                                                         :link/managed? true
                                                         :link/render-inline? true
                                                         :link/tx-fn (:entity-remove txfn-lookup)}]
                                             (case (:fiddle/type parent-fiddle)
                                               :entity [remove]

                                               :query [edit new remove]

                                               :blank [])))))
                          (apply concat)
                          doall)

        attr-links (if (not= :blank (:fiddle/type parent-fiddle))
                     (->> ordered-fes
                          (map-indexed (fn [fe-pos fe]
                                         (when (:source-symbol fe)
                                           (let [schema (get-in schemas [(str (:source-symbol fe))])]
                                             (->> (:fields fe)
                                                  (filter (fn [{:keys [attribute]}]
                                                            (and (not= attribute :db/id)
                                                                 (= :db.type/ref (get-in schema [attribute :db/valueType :db/ident])))))
                                                  (mapcat (fn [{:keys [attribute]}]
                                                            [{:db/id {:ident :system-anchor-edit-attr :fe fe :a attribute}
                                                              :hypercrud/sys? true
                                                              :anchor/prompt (str "edit") ; conserve space in label
                                                              :link/rel (keyword (str "sys-edit-" (:name fe) "-" attribute))
                                                              :link/dependent? true
                                                              :link/path (str fe-pos " " attribute)
                                                              :link/managed? false
                                                              :link/disabled? true
                                                              :link/fiddle system-fiddle/fiddle-system-edit}
                                                             {:db/id {:ident :system-anchor-new-attr :fe fe :a attribute}
                                                              :hypercrud/sys? true
                                                              :anchor/prompt (str "new") ; conserve space in label
                                                              :link/rel (keyword (str "sys-new-" (:name fe) "-" attribute))
                                                              :link/dependent? true ; manged - need parent-child ref
                                                              :link/path (str fe-pos " " attribute)
                                                              :link/managed? true
                                                              :link/create? true
                                                              :link/render-inline? true
                                                              :link/disabled? true
                                                              :link/fiddle system-fiddle/fiddle-system-edit}
                                                             {:db/id {:ident :system-anchor-remove-attr :fe fe :a attribute}
                                                              :hypercrud/sys? true
                                                              :anchor/prompt (str "remove")
                                                              :link/rel (keyword (str "sys-remove-" (:name fe) "-" attribute))
                                                              :link/fiddle system-fiddle/fiddle-blank-system-remove
                                                              :link/path (str fe-pos " " attribute)
                                                              :link/dependent? true
                                                              :link/managed? true
                                                              :link/render-inline? true
                                                              :link/disabled? true
                                                              :link/tx-fn (if (= :db.cardinality/one (get-in schema [attribute :db/cardinality :db/ident]))
                                                                            (:value-remove-one txfn-lookup)
                                                                            (:value-remove-many txfn-lookup))}])))))))
                          (apply concat)
                          doall))]
    (concat entity-links attr-links)))
