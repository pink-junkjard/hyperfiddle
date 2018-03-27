(ns hypercrud.browser.auto-fiddle                           ; system-fiddle
  (:require [hypercrud.browser.schema-attribute :as schema-attribute]
            [hypercrud.compile.macros :refer [str-and-code]]
            [hypercrud.util.non-fatal :refer [try-either]]
            [clojure.string :as str]))


(defn system-fiddle? [fiddle-id]
  (and (keyword? fiddle-id)                                 ; why long here wut?
       (namespace fiddle-id)
       (str/starts-with? (namespace fiddle-id) "hyperfiddle.system")))

; these need to be thick/hydrated params bc we are manufacturing a pulled tree here.

(def fiddle-system-edit
  {:fiddle/ident :hyperfiddle.system/edit
   :fiddle/type :entity})

(defn fiddle-blank-system-remove []
  {:fiddle/ident :hyperfiddle.system/remove
   :fiddle/type :blank
   :fiddle/renderer (str-and-code
                      (fn [result fes anchors ctx]
                        [:p "Retract entity?"]))})

(defn schema-cardinality-options [$db]
  {:fiddle/ident (keyword "hyperfiddle.system.schema.cardinality-options" $db)
   :fiddle/query (str [:find (list 'pull $db '?e [:db/id :db/ident])
                       :in $db
                       :where
                       [$db '?e :db/ident '?ident]
                       '[(namespace ?ident) ?ns]
                       '[(= ?ns "db.cardinality")]])
   :fiddle/type :query})

(defn schema-unique-options [$db]
  {:fiddle/ident (keyword "hyperfiddle.system.schema.unique-options" $db)
   :fiddle/query (str [:find (list 'pull $db '?e [:db/id :db/ident])
                       :in $db
                       :where
                       [$db '?e :db/ident '?ident]
                       '[(namespace ?ident) ?ns]
                       '[(= ?ns "db.unique")]])
   :fiddle/type :query})

(defn schema-valueType-options [$db]
  {:fiddle/ident (keyword "hyperfiddle.system.schema.valueType-options" $db)
   :fiddle/query (str [:find (list 'pull $db '?valueType [:db/id :db/ident])
                       :in $db
                       :where
                       [$db '?db-part :db.install/valueType '?valueType]
                       [$db '?db-part :db/ident :db.part/db]])
   :fiddle/type :query})

(defn schema-attribute [$db]
  {:fiddle/ident (keyword "hyperfiddle.system.schema.attribute" (str $db))
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
   :fiddle/links #{{:db/id (keyword "hyperfiddle.system.schema.cardinality-options" $db)
                    :link/fiddle (schema-cardinality-options $db)
                    :link/render-inline? true
                    :link/rel :options
                    :link/path "0 :db/cardinality"}
                   {:db/id (keyword "hyperfiddle.system.schema.unique-options" $db)
                    :link/fiddle (schema-unique-options $db)
                    :link/render-inline? true
                    :link/rel :options
                    :link/path "0 :db/unique"}
                   {:db/id (keyword "hyperfiddle.system.schema.valueType-options" $db)
                    :link/fiddle (schema-valueType-options $db)
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
  (defn schema-all-attributes [$db]
    {:fiddle/ident (keyword "hyperfiddle.system.schema.all-attributes" $db)
     :fiddle/query (str [:find [(list 'pull $db '?attr [:db/id :db/ident :db/valueType :db/cardinality :db/doc :db/unique :db/isComponent :db/fulltext]) '...]
                         :in $db
                         :where [$db :db.part/db :db.install/attribute '?attr]])
     :fiddle/type :query
     :fiddle/bindings (str-and-code (fn [param-ctx] (assoc param-ctx :read-only (constantly true))))
     :fiddle/renderer renderer
     :fiddle/links #{{:db/id (keyword "hyperfiddle.system.schema.cardinality-options-link" $db)
                      :link/fiddle (schema-cardinality-options $db)
                      :link/render-inline? true
                      :link/rel :options
                      :link/path "0 :db/cardinality"}
                     {:db/id (keyword "hyperfiddle.system.schema.unique-options-link" $db)
                      :link/fiddle (schema-unique-options $db)
                      :link/render-inline? true
                      :link/rel :options
                      :link/path "0 :db/unique"}
                     {:db/id (keyword "hyperfiddle.system.schema.valueType-options" $db)
                      :link/fiddle (schema-valueType-options $db)
                      :link/render-inline? true
                      :link/rel :options
                      :link/path "0 :db/valueType"}
                     {:db/id :system-anchor-edit            ; XXX
                      :link/rel :sys-edit-?attr
                      :link/fiddle (schema-attribute $db)}
                     {:db/id :system-anchor-new             ; XXX
                      :link/rel :sys-new-?attr
                      :link/fiddle (schema-attribute $db)}
                     {:db/id :system-anchor-remove          ; XXX
                      :link/rel :sys-remove-?attr
                      :link/disabled? true}}}))

(defn hydrate-system-fiddle [fiddle-id]
  (try-either                                               ; catch all the pre assertions
    (cond
      (= fiddle-id :hyperfiddle.system/edit) fiddle-system-edit
      (= fiddle-id :hyperfiddle.system/remove) fiddle-blank-system-remove
      :else (let [$db (symbol (name fiddle-id))]
              (condp = (namespace fiddle-id)
                "hyperfiddle.system.schema.all-attributes" (schema-all-attributes $db)
                "hyperfiddle.system.schema.cardinality-options" (schema-cardinality-options $db)
                "hyperfiddle.system.schema.unique-options" (schema-unique-options $db)
                "hyperfiddle.system.schema.valueType-options" (schema-valueType-options $db)
                "hyperfiddle.system.schema.attribute" (schema-attribute $db))))))
