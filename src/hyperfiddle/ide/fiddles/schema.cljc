(ns hyperfiddle.ide.fiddles.schema
  (:require [contrib.data :refer [mpprint-str pprint-str]]
            [contrib.macros :refer [str-and-code]]
            [hyperfiddle.ide.fiddles.schema-attribute :as schema-attribute]
            [hypercrud.browser.context :as context]
            [cuerdas.core :as str]))


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
                    :link/rel :hyperfiddle/remove
                    :link/path "0"
                    :link/disabled? true}}})

(let [renderer (mpprint-str
                 '(fn [ctx]
                    (let [hide-datomic (reagent.core/atom true)
                          hide-archived (reagent.core/atom true)
                          db-attr? #(<= (:db/id %) 62)
                          archived? #(cuerdas.core/starts-with? (namespace (:db/ident %)) "zzz") ; "zzz/" and "zzz.", we are inconsistent. It should be modeled and queried and never shown
                          do-filter-reactive (fn [xs] ; perf sensitive
                                               (as-> xs xs
                                                     (if @hide-datomic (remove db-attr? xs) xs)
                                                     (if @hide-archived (remove archived? xs) xs)))]
                      (fn [ctx]
                        [:div.hyperfiddle-schema
                         #_(hypercrud.ui.result/ident ctx)
                         (hypercrud.ui.result/doc ctx)
                         [:label {:style {:font-weight "400" :display "block"}} [:input {:type "checkbox" :checked @hide-datomic :on-change #(swap! hide-datomic not)}] " hide Datomic system attributes"]
                         [:label {:style {:font-weight "400" :display "block"}} [:input {:type "checkbox" :checked @hide-archived :on-change #(swap! hide-archived not)}] " hide Hyperfiddle archived attributes"]
                         (let [ctx (-> ctx
                                       (dissoc :relation :relations)
                                       (update :hypercrud.browser/result (partial contrib.reactive/fmap do-filter-reactive #_(contrib.reactive/partial filter f?)))
                                       (hypercrud.browser.context/with-relations))]
                           [hypercrud.ui.result/result ctx])]))))]
  (defn schema [$db]
    {:fiddle/ident (keyword "hyperfiddle.schema" $db)
     :db/doc (str "### Datomic schema for " $db)
     :fiddle/query (let [$db (symbol $db)]
                     (pprint-str [:in $db :find [(list 'pull $db '?attr [:db/id :db/ident :db/valueType :db/cardinality :db/doc :db/unique :db/isComponent :db/fulltext]) '...]
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
                      :link/rel :hyperfiddle/edit
                      :link/path "0"
                      :link/fiddle (db-attribute-edit $db)}
                     {:db/id :system-anchor-new             ; XXX
                      :link/rel :hyperfiddle/new
                      :link/path "0"
                      :link/fiddle (db-attribute-edit $db)}
                     {:db/id :system-anchor-remove          ; XXX
                      :link/rel :hyperfiddle/remove
                      :link/path "0"
                      :link/disabled? true}}}))
