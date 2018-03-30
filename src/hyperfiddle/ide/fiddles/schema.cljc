(ns hyperfiddle.ide.fiddles.schema
  (:require [hyperfiddle.ide.fiddles.schema-attribute :as schema-attribute]
            [contrib.macros :refer [str-and-code]]))


(defn db-cardinality-options [$db]
  {:fiddle/ident (keyword "hyperfiddle.schema.db-cardinality-options" $db)
   :fiddle/type :query
   :fiddle/query (let [$db (symbol $db)]
                   (str [:in $db :find (list 'pull $db '?e [:db/id :db/ident]) :where
                         [$db '?e :db/ident '?ident]
                         '[(namespace ?ident) ?ns]
                         '[(= ?ns "db.cardinality")]]))})

(defn db-unique-options [$db]
  {:fiddle/ident (keyword "hyperfiddle.schema.db-unique-options" $db)
   :fiddle/type :query
   :fiddle/query (let [$db (symbol $db)]
                   (str [:in $db :find (list 'pull $db '?e [:db/id :db/ident]) :where
                         [$db '?e :db/ident '?ident]
                         '[(namespace ?ident) ?ns]
                         '[(= ?ns "db.unique")]]))})

(defn db-valueType-options [$db]
  {:fiddle/ident (keyword "hyperfiddle.schema.db-valueType-options" $db)
   :fiddle/type :query
   :fiddle/query (let [$db (symbol $db)]
                   (str [:in $db :find (list 'pull $db '?valueType [:db/id :db/ident]) :where
                         [$db '?db-part :db.install/valueType '?valueType]
                         [$db '?db-part :db/ident :db.part/db]]))})

(defn db-attribute-edit [$db]
  {:fiddle/ident (keyword "hyperfiddle.schema.db-attribute-edit" $db)
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
   :fiddle/links #{{:db/id (keyword "hyperfiddle.schema.db-cardinality-options" $db)
                    :link/fiddle (db-cardinality-options $db)
                    :link/render-inline? true
                    :link/rel :options
                    :link/path "0 :db/cardinality"}
                   {:db/id (keyword "hyperfiddle.schema.db-unique-options" $db)
                    :link/fiddle (db-unique-options $db)
                    :link/render-inline? true
                    :link/rel :options
                    :link/path "0 :db/unique"}
                   {:db/id (keyword "hyperfiddle.schema.db-valueType-options" $db)
                    :link/fiddle (db-valueType-options $db)
                    :link/render-inline? true
                    :link/rel :options
                    :link/path "0 :db/valueType"}
                   {:db/id :system-anchor-remove            ; XXX
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
  (defn schema [$db]
    {:fiddle/ident (keyword "hyperfiddle.schema" $db)
     :fiddle/query (let [$db (symbol $db)]
                     (str [:find [(list 'pull $db '?attr [:db/id :db/ident :db/valueType :db/cardinality :db/doc :db/unique :db/isComponent :db/fulltext]) '...]
                           :in $db
                           :where [$db :db.part/db :db.install/attribute '?attr]]))
     :fiddle/type :query
     :fiddle/bindings (str-and-code (fn [param-ctx] (assoc param-ctx :read-only (constantly true))))
     :fiddle/renderer renderer
     :fiddle/links #{{:db/id (keyword "hyperfiddle.schema.db-cardinality-options-link" $db)
                      :link/fiddle (db-cardinality-options $db)
                      :link/render-inline? true
                      :link/rel :options
                      :link/path "0 :db/cardinality"}
                     {:db/id (keyword "hyperfiddle.schema.db-unique-options-link" $db)
                      :link/fiddle (db-unique-options $db)
                      :link/render-inline? true
                      :link/rel :options
                      :link/path "0 :db/unique"}
                     {:db/id (keyword "hyperfiddle.schema.db-valueType-options" $db)
                      :link/fiddle (db-valueType-options $db)
                      :link/render-inline? true
                      :link/rel :options
                      :link/path "0 :db/valueType"}
                     {:db/id :system-anchor-edit            ; XXX
                      :link/rel :sys-edit-?attr
                      :link/fiddle (db-attribute-edit $db)}
                     {:db/id :system-anchor-new             ; XXX
                      :link/rel :sys-new-?attr
                      :link/fiddle (db-attribute-edit $db)}
                     {:db/id :system-anchor-remove          ; XXX
                      :link/rel :sys-remove-?attr
                      :link/disabled? true}}}))
