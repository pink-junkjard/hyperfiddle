(ns hypercrud.browser.system-link                           ; this namespace is public extern
  (:require [clojure.string :as str]
            [contrib.vedn :as vedn]
            [hypercrud.browser.system-fiddle :as system-fiddle]))


(defn ^:export system-link? [link-id]
  (and (keyword? link-id)
       (some-> (namespace link-id) (str/starts-with? "hyperfiddle.browser.system-link"))))

(def txfn-lookup (vedn/load-vedn-from-file "system-link/tx-fns.vedn"))

(defn system-links
  "All sys links are :link/rel :sys, so they can be matched and merged with user-links.
  Matching is determined by [repeat? entity attribute ident]

  This appfn recurses in unexpected abstract way and impacts performance highly
  "
  [parent-fiddle ordered-fes schemas]
  (let [entity-links (->> ordered-fes
                          (map-indexed (fn [fe-pos fe]
                                         (when-let [dbname (some-> (:source-symbol fe) str)]
                                           (let [; create links mirror edit links but repeating false, see auto-formula.
                                                 ; This is because the connection comes from the find-element, and when merging
                                                 ; sys links we match on the find-element.
                                                 new {:db/id (keyword "hyperfiddle.browser.system-link" (str "new-" (:name fe)))
                                                      :hypercrud/sys? true
                                                      :link/rel :hyperfiddle/new
                                                      :link/fiddle (system-fiddle/fiddle-system-edit dbname)
                                                      :link/dependent? false ; not managed, no parent-child ref
                                                      :link/path (str fe-pos)
                                                      :link/managed? true
                                                      :link/create? true
                                                      :link/render-inline? true}]
                                             (if (:entity fe)
                                               (let [edit {:db/id (keyword "hyperfiddle.browser.system-link" (str "edit-" (:name fe)))
                                                           :hypercrud/sys? true
                                                           :link/rel :hyperfiddle/edit
                                                           :link/fiddle (system-fiddle/fiddle-system-edit dbname)
                                                           :link/dependent? true
                                                           :link/managed? false
                                                           :link/path (str fe-pos)}
                                                     remove {:db/id (keyword "hyperfiddle.browser.system-link" (str "remove-" (:name fe)))
                                                             :hypercrud/sys? true
                                                             :link/rel :hyperfiddle/remove
                                                             :link/fiddle system-fiddle/fiddle-blank-system-remove
                                                             :link/dependent? true
                                                             :link/path (str fe-pos)
                                                             :link/managed? true
                                                             :link/render-inline? true
                                                             :link/tx-fn (:entity-remove txfn-lookup)}]
                                                 (case (:fiddle/type parent-fiddle)
                                                   :entity [remove]
                                                   :query [edit new remove]
                                                   :blank []))
                                               (case (:fiddle/type parent-fiddle)
                                                 :entity []
                                                 :query [new]
                                                 :blank []))))))
                          (apply concat)
                          doall)

        attr-links (if (not= :blank (:fiddle/type parent-fiddle))
                     (->> ordered-fes
                          (map-indexed (fn [fe-pos fe]
                                         (when-let [dbname (some-> (:source-symbol fe) str)]
                                           (let [schema (get schemas dbname)]
                                             (->> (:fields fe)
                                                  (filter (fn [{:keys [attribute]}]
                                                            (and (not= attribute :db/id)
                                                                 (= :db.type/ref (get-in schema [attribute :db/valueType :db/ident])))))
                                                  (mapcat (fn [{:keys [attribute]}]
                                                            (let [edit {:db/id (keyword "hyperfiddle.browser.system-link" (str "edit-" (hash [(:name fe) attribute])))
                                                                        :hypercrud/sys? true
                                                                        :link/rel :hyperfiddle/edit
                                                                        :link/dependent? true
                                                                        :link/path (str fe-pos " " attribute)
                                                                        :link/managed? false
                                                                        :link/disabled? true
                                                                        :link/fiddle (system-fiddle/fiddle-system-edit dbname)}]
                                                              (if (:entity fe)
                                                                (let [new {:db/id (keyword "hyperfiddle.browser.system-link" (str "new-" (hash [(:name fe) attribute])))
                                                                           :hypercrud/sys? true
                                                                           :link/rel :hyperfiddle/new
                                                                           :link/dependent? true ; manged - need parent-child ref
                                                                           :link/path (str fe-pos " " attribute)
                                                                           :link/managed? true
                                                                           :link/create? true
                                                                           :link/render-inline? true
                                                                           :link/disabled? true
                                                                           :link/fiddle (system-fiddle/fiddle-system-edit dbname)}
                                                                      remove {:db/id (keyword "hyperfiddle.browser.system-link" (str "remove-" (hash [(:name fe) attribute])))
                                                                              :hypercrud/sys? true
                                                                              :link/rel :hyperfiddle/remove
                                                                              :link/fiddle system-fiddle/fiddle-blank-system-remove
                                                                              :link/path (str fe-pos " " attribute)
                                                                              :link/dependent? true
                                                                              :link/managed? true
                                                                              :link/render-inline? true
                                                                              :link/disabled? true
                                                                              :link/tx-fn (if (= :db.cardinality/one (get-in schema [attribute :db/cardinality :db/ident]))
                                                                                            (:value-remove-one txfn-lookup)
                                                                                            (:value-remove-many txfn-lookup))}]
                                                                  [edit new remove])
                                                                [edit])))))))))
                          (apply concat)
                          doall))]
    (concat entity-links attr-links)))
