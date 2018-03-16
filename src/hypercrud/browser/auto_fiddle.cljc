(ns hypercrud.browser.auto-fiddle
  (:require [hypercrud.browser.schema-attribute :as schema-attribute]
            [hypercrud.compile.macros :refer [str-and-code]]
            [hypercrud.util.non-fatal :refer [try-either]]))


(defn system-fiddle? [fiddle-id]
  (or (map? fiddle-id)
      (and (or (seq? fiddle-id) (vector? fiddle-id))        ; lookup ref?
           (= :fiddle/ident (first fiddle-id))
           (map? (second fiddle-id)))))

(defn display-fiddle-ident [fiddle-id]
  (cond
    (map? fiddle-id) (:ident fiddle-id)
    (and (or (seq? fiddle-id) (vector? fiddle-id))          ; lookup ref?
         (= :fiddle/ident (first fiddle-id))) (second fiddle-id)
    :else fiddle-id))

(defn fiddle-system-edit [fe-name]                          ; these need to be thick/hydrated params bc we are manufacturing a pulled tree here.
  {:pre [fe-name]}
  {:fiddle/ident {:ident :system-edit
                  :fe-name fe-name}
   :fiddle/type :entity})

(defn fiddle-system-edit-attr [fe-name a]
  {:pre [fe-name a]}
  {:fiddle/ident {:ident :system-edit-attr
                  :fe-name fe-name
                  :a a}
   :fiddle/type :entity})

(defn fiddle-blank-system-remove [fe-name a]
  {:fiddle/ident {:ident :sys-remove
                  :fe-name fe-name
                  :a a}
   :fiddle/type :blank
   :fiddle/renderer (str-and-code
                      (fn [result fes anchors ctx]
                        [:p "Retract entity?"]))})

(defn schema-cardinality-options [$db]
  {:fiddle/ident {:ident :schema/cardinality-options
                  :dbhole/name $db}
   :fiddle/query (str [:find (list 'pull $db '?e [:db/id :db/ident])
                       :in $db
                       :where
                       [$db '?e :db/ident '?ident]
                       '[(namespace ?ident) ?ns]
                       '[(= ?ns "db.cardinality")]])
   :fiddle/type :query})

(defn schema-unique-options [$db]
  {:fiddle/ident {:ident :schema/unique-options
                  :dbhole/name $db}
   :fiddle/query (str [:find (list 'pull $db '?e [:db/id :db/ident])
                       :in $db
                       :where
                       [$db '?e :db/ident '?ident]
                       '[(namespace ?ident) ?ns]
                       '[(= ?ns "db.unique")]])
   :fiddle/type :query})

(defn schema-valueType-options [$db]
  {:fiddle/ident {:ident :schema/valueType-options
                  :dbhole/name $db}
   :fiddle/query (str [:find (list 'pull $db '?valueType [:db/id :db/ident])
                       :in $db
                       :where
                       [$db '?db-part :db.install/valueType '?valueType]
                       [$db '?db-part :db/ident :db.part/db]])
   :fiddle/type :query})

(defn schema-attribute [$db]
  {:fiddle/ident {:ident :schema/attribute
                  :dbhole/name $db}
   :fiddle/type :entity
   :fiddle/pull (str [[:db/id
                       :db/ident
                       :db/valueType
                       :db/cardinality
                       :db/doc
                       :db/unique
                       :db/isComponent
                       :db/fulltext]])
   :fiddle/renderer (str `schema-attribute/renderer)
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
                   {:db/id {:ident :system-anchor-remove
                            :fe "entity"}
                    :link/rel :sys-remove-entity
                    :link/disabled? true}}})

(let [renderer (str
                 '(fn [ctx]
                    (let [hide-datomic? (reagent.core/atom true)
                          datomic-filter (fn [attributes]
                                           (if @hide-datomic?
                                             (filter #(> (:db/id %) 62) attributes)
                                             attributes))]
                      (fn [ctx]
                        [:div.hyperfiddle-attributes
                         [:label {:for "hide-datomic"}
                          [:input {:type "checkbox"
                                   :id "hide-datomic"
                                   :checked @hide-datomic?
                                   :on-change #(swap! hide-datomic? not)}]
                          " Hide Datomic attributes?"]
                         (let [ctx (update ctx :hypercrud.browser/result (partial hypercrud.util.reactive/fmap datomic-filter))]
                           [hypercrud.ui.result/view ctx])]))))]
  (defn schema-all-attributes [$db]
    {:fiddle/ident {:ident :schema/all-attributes
                    :dbhole/name $db}
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

(defn hydrate-system-fiddle [fiddle-id]
  (let [id (second fiddle-id)]
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
        :sys-remove (fiddle-blank-system-remove (:fe-name id) (:a id))))))

