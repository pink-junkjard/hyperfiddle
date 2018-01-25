(ns hypercrud.browser.auto-fiddle
  (:require [hypercrud.compile.macros :refer [str-and-code]]
            [hypercrud.util.non-fatal :refer [try-either]]))


(defn system-fiddle? [fiddle-id]
  (map? fiddle-id))

(defn fiddle-system-edit [fe-name]                          ; these need to be thick/hydrated params bc we are manufacturing a pulled tree here.
  {:pre [fe-name]}
  {:db/id {:ident :system-edit
           :fe-name fe-name}
   :fiddle/name (str "system-" fe-name)
   :fiddle/type :entity})

(defn fiddle-system-edit-attr [fe-name a]
  {:pre [fe-name a]}
  {:db/id {:ident :system-edit-attr
           :fe-name fe-name
           :a a}
   :fiddle/name (str "system-" fe-name "-" a)
   :fiddle/type :entity})

(defn fiddle-blank-system-remove [fe-name a]
  {:db/id {:ident :sys-remove
           :fe-name fe-name
           :a a}
   :fiddle/name "sys-remove"
   :fiddle/type :blank
   :fiddle/renderer (pr-str `(fn [result# ordered-fes# anchors# ctx#]
                               [:p "Retract entity?"]))})

(defn schema-cardinality-options [$db]
  {:db/id {:ident :schema/cardinality-options
           :dbhole/name $db}
   :fiddle/name (str "[schema] " $db " :db/cardinality Options")
   :fiddle/query (str [:find (list 'pull $db '?e [:db/id :db/ident])
                       :in $db
                       :where
                       [$db '?e :db/ident '?ident]
                       '[(namespace ?ident) ?ns]
                       '[(= ?ns "db.cardinality")]])
   :fiddle/type :query})

(defn schema-unique-options [$db]
  {:db/id {:ident :schema/unique-options
           :dbhole/name $db}
   :fiddle/name (str "[schema] " $db " :db/unique Options")
   :fiddle/query (str [:find (list 'pull $db '?e [:db/id :db/ident])
                       :in $db
                       :where
                       [$db '?e :db/ident '?ident]]
                      '[(namespace ?ident) ?ns]
                      '[(= ?ns "db.unique")])

   :fiddle/type :query})

(defn schema-valueType-options [$db]
  {:db/id {:ident :schema/valueType-options
           :dbhole/name $db}
   :fiddle/name (str "[schema] " $db " :db/valueType Options")
   :fiddle/query (str [:find (list 'pull $db '?valueType [:db/id :db/ident])
                       :in $db
                       :where
                       [$db '?db-part :db.install/valueType '?valueType]
                       [$db '?db-part :db/ident :db.part/db]])
   :fiddle/type :query})

(let [renderer (str
                 '(let [special-case-attrs #{:db/ident :db/cardinality :db/valueType}
                        has-required-attrs? (fn [entity] (clojure.set/subset? special-case-attrs (set (keys entity))))
                        read-only (fn [attr ctx]
                                    (not (or (has-required-attrs? @(:cell-data ctx))
                                             (#{:db/ident :db/doc :db/valueType :db/cardinality} (:db/ident attr)))))
                        merge-in-tx (fn [entity tx ctx]
                                      (reduce (fn [entity [op e a v]]
                                                ; todo this fn has bare minimum support for this page
                                                ; e.g. doesnt support card/many or nested modals
                                                (let [attr (get-in ctx [:schemas "$" a])
                                                      v (if (= :db.type/ref (-> attr :db/valueType :db/ident))
                                                          {:db/id v}
                                                          v)]
                                                  (case op
                                                    :db/add (assoc entity a v)
                                                    :db/retract (dissoc entity a))))
                                              (into {} entity)
                                              tx))]
                    (fn [result colspec anchors ctx]
                      (let [special-attrs-state (reagent.core/atom nil)

                            valueType-and-cardinality-renderer
                            (let [user-with! (fn [ctx user-with! tx]
                                               (let [entity @(:cell-data ctx)
                                                     new-entity (merge-in-tx entity tx ctx)]
                                                 (case [(has-required-attrs? entity) (has-required-attrs? new-entity)]
                                                   [false false]
                                                   (swap! special-attrs-state hypercrud.client.tx/into-tx tx)

                                                   [false true]
                                                   (do
                                                     (user-with! (hypercrud.client.tx/into-tx @special-attrs-state tx))
                                                     (reset! special-attrs-state nil))

                                                   [true false]
                                                   ; todo this case WILL throw (going from a valid tx to invalid)
                                                   (user-with! tx)

                                                   [true true]
                                                   (user-with! tx))))]
                              (hypercrud.compile.macros/str-and-code'
                                (fn [field props ctx]
                                  (let [ctx (update ctx :user-with! #(reagent.core/partial user-with! ctx %))]
                                    (hypercrud.ui.auto-control/auto-control field nil props ctx)))
                                "todo"))

                            ident-renderer
                            (let [user-with! (fn [ctx user-with! tx]
                                               (let [entity @(:cell-data ctx)
                                                     new-entity (merge-in-tx entity tx ctx)]
                                                 (case [(has-required-attrs? entity) (has-required-attrs? new-entity)]
                                                   [false false]
                                                   (user-with! tx)

                                                   [false true]
                                                   (do
                                                     (user-with! (hypercrud.client.tx/into-tx @special-attrs-state tx))
                                                     (reset! special-attrs-state nil))

                                                   [true false]
                                                   ; todo this case WILL throw (going from a valid tx to invalid)
                                                   (user-with! tx)

                                                   [true true]
                                                   (user-with! tx))))]
                              (hypercrud.compile.macros/str-and-code'
                                (fn [field props ctx]
                                  (let [ctx (update ctx :user-with! #(reagent.core/partial user-with! ctx %))]
                                    (hypercrud.ui.auto-control/auto-control field nil props ctx)))
                                "todo"))]
                        (fn [entity colspec anchors ctx]
                          (let [entity (merge-in-tx entity @special-attrs-state ctx)
                                ctx (-> ctx
                                        (assoc :read-only read-only)
                                        (assoc-in [:fields :db/cardinality :renderer] valueType-and-cardinality-renderer)
                                        (assoc-in [:fields :db/valueType :renderer] valueType-and-cardinality-renderer)
                                        (assoc-in [:fields :db/ident :renderer] ident-renderer))]
                            (hypercrud.ui.result/view entity colspec anchors ctx)))))))]
  (defn schema-attribute [$db]
    {:db/id {:ident :schema/attribute
             :dbhole/name $db}
     :fiddle/name (str "[schema] " $db " attribute")
     :fiddle/type :entity
     :fiddle/pull (str [[:db/id
                         :db/ident
                         :db/valueType
                         :db/cardinality
                         :db/doc
                         :db/unique
                         :db/isComponent
                         :db/fulltext]])
     :fiddle/renderer renderer
     :fiddle/links #{{:db/id {:ident :schema/cardinality-options-link}
                      :link/fiddle (schema-cardinality-options $db)
                      :link/dependent? true
                      :link/render-inline? true
                      :link/rel :options
                      :link/path "0 :db/cardinality"}
                     {:db/id {:ident :schema/unique-options-link}
                      :link/fiddle (schema-unique-options $db)
                      :link/dependent? true
                      :link/render-inline? true
                      :link/rel :options
                      :link/path "0 :db/unique"}
                     {:db/id {:ident :schema/valueType-options-link}
                      :link/fiddle (schema-valueType-options $db)
                      :link/dependent? true
                      :link/render-inline? true
                      :link/rel :options
                      :link/path "0 :db/valueType"}
                     {:db/id {:ident :system-anchor-remove
                              :fe "entity"}
                      :link/rel :sys-remove-entity
                      :link/disabled? true}}}))

(let [renderer (str
                 '(fn [attributes ordered-fes anchors ctx]
                    (let [hide-datomic? (reagent.core/atom true)]
                      (fn [attributes ordered-fes anchors ctx]
                        (let [attributes (if @hide-datomic?
                                           (filter #(> (:db/id %) 62) attributes)
                                           attributes)]
                          [:div.hyperfiddle-attributes
                           [:label {:for "hide-datomic"}
                            [:input {:type "checkbox"
                                     :id "hide-datomic"
                                     :checked @hide-datomic?
                                     :on-change #(swap! hide-datomic? not)}]
                            " Hide Datomic attributes?"]
                           [hypercrud.ui.result/view attributes ordered-fes anchors ctx]
                           [:style {:dangerouslySetInnerHTML {:__html ".hyperfiddle-attributes { max-height: 40vh; }"}}]])))))]
  (defn schema-all-attributes [$db]
    {:db/id {:ident :schema/all-attributes
             :dbhole/name $db}
     :fiddle/name (str "[schema] " $db " attributes")
     :fiddle/query (str [:find [(list 'pull $db '?attr [:db/id :db/ident :db/valueType :db/cardinality :db/doc :db/unique :db/isComponent :db/fulltext]) '...]
                         :in $db
                         :where [$db :db.part/db :db.install/attribute '?attr]])
     :fiddle/type :query
     :fiddle/bindings (str-and-code (fn [param-ctx] (assoc param-ctx :read-only (constantly true))))
     :fiddle/renderer renderer
     :fiddle/links #{{:db/id {:ident :schema/cardinality-options-link}
                      :link/fiddle (schema-cardinality-options $db)
                      :link/render-inline? true
                      :link/rel :options
                      :link/path "0 :db/cardinality"}
                     {:db/id {:ident :schema/unique-options-link}
                      :link/fiddle (schema-unique-options $db)
                      :link/render-inline? true
                      :link/rel :options
                      :link/path "0 :db/unique"}
                     {:db/id {:ident :schema/valueType-options-link}
                      :link/fiddle (schema-valueType-options $db)
                      :link/render-inline? true
                      :link/rel :options
                      :link/path "0 :db/valueType"}
                     {:db/id {:ident :system-anchor-edit
                              :fe "?attr"}
                      :link/rel :sys-edit-?attr
                      :link/fiddle (schema-attribute $db)}
                     {:db/id {:ident :system-anchor-new
                              :fe "?attr"}
                      :link/rel :sys-new-?attr
                      :link/fiddle (schema-attribute $db)}
                     {:db/id {:ident :system-anchor-remove
                              :fe "?attr"}
                      :link/rel :sys-remove-?attr
                      :link/disabled? true}}}))

(defn hydrate-system-fiddle [id]
  (try-either
    ; catch all the pre assertions
    (case (:ident id)
      :schema/all-attributes (schema-all-attributes (:dbhole/name id))
      :schema/cardinality-options (schema-cardinality-options (:dbhole/name id))
      :schema/unique-options (schema-unique-options (:dbhole/name id))
      :schema/valueType-options (schema-valueType-options (:dbhole/name id))
      :schema/attribute (schema-attribute (:dbhole/name id))
      :system-edit (fiddle-system-edit (:fe-name id))
      :system-edit-attr (fiddle-system-edit-attr (:fe-name id) (:a id))
      :sys-remove (fiddle-blank-system-remove (:fe-name id) (:a id)))))
