(ns hyperfiddle.ide.fiddles.schema
  (:require
    [contrib.template :refer [load-resource]]
    [contrib.pprint :refer [mpprint-str pprint-str]]))


(defn db-cardinality-options [$db]
  {:fiddle/ident (keyword "hyperfiddle.schema.db-cardinality-options" $db)
   :fiddle/type :query
   :fiddle/query (let [$db (symbol $db)]
                   (str [:in $db :find [(list 'pull $db '?e [:db/ident]) '...] :where
                         [$db '?e :db/ident '?ident]
                         '[(namespace ?ident) ?ns]
                         '[(= ?ns "db.cardinality")]]))})

(defn db-unique-options [$db]
  {:fiddle/ident (keyword "hyperfiddle.schema.db-unique-options" $db)
   :fiddle/type :query
   :fiddle/query (let [$db (symbol $db)]
                   (str [:in $db :find [(list 'pull $db '?e [:db/ident]) '...] :where
                         [$db '?e :db/ident '?ident]
                         '[(namespace ?ident) ?ns]
                         '[(= ?ns "db.unique")]]))})

(defn db-valueType-options [$db]
  {:fiddle/ident (keyword "hyperfiddle.schema.db-valueType-options" $db)
   :fiddle/type :query
   :fiddle/query (let [$db (symbol $db)]
                   (str [:in $db :find [(list 'pull $db '?valueType [:db/ident]) '...] :where
                         [$db '?db-part :db.install/valueType '?valueType]
                         [$db '?db-part :db/ident :db.part/db]]))})

(defn db-attribute-edit [$db]
  {:fiddle/ident (keyword "hyperfiddle.schema.db-attribute-edit" $db)
   :fiddle/type :entity
   :fiddle/pull (str [:db/id                                ; for smart-identity tempid stability
                      :db/ident
                      {:db/valueType [:db/ident]}
                      {:db/cardinality [:db/ident]}
                      :db/doc
                      :db/unique
                      :db/isComponent
                      :db/fulltext])
   :fiddle/pull-database $db
   :fiddle/renderer (str '[hyperfiddle.ide.fiddles.schema-attribute/renderer val ctx props])
   :fiddle/links #{{:db/id (keyword "hyperfiddle.schema.db-cardinality-options" $db)
                    :link/fiddle (db-cardinality-options $db)
                    :link/rel :hf/iframe
                    :link/class #{:cardinality-options}}
                   {:db/id (keyword "hyperfiddle.schema.db-unique-options" $db)
                    :link/fiddle (db-unique-options $db)
                    :link/rel :hf/iframe
                    :link/class #{:unique-options}}
                   {:db/id (keyword "hyperfiddle.schema.db-valueType-options" $db)
                    :link/fiddle (db-valueType-options $db)
                    :link/rel :hf/iframe
                    :link/class #{:valueType-options}}
                   {:db/id :system-anchor-remove            ; XXX
                    :link/rel :hf/remove}}})

(defn schema [$db]
  {:fiddle/ident (keyword "hyperfiddle.schema" $db)
   :db/doc (str "### Datomic schema for " $db)
   :fiddle/query (let [$db (symbol $db)]
                   ; pprint-str is slow and its runtime generated so can't define it as a .edn resource with whitespace
                   ; Real problem is that userland can even see this string - its a sys fiddle, you shouldn't see it in the IDE
                   (str [:in $db :find [(list 'pull $db '?attr
                                              [:db/id
                                               :db/ident
                                               {:db/valueType [:db/ident]}
                                               {:db/cardinality [:db/ident]}
                                               {:db/unique [:db/ident]}
                                               :db/isComponent :db/fulltext :db/doc]) '...]
                         :where [$db :db.part/db :db.install/attribute '?attr]]))
   :fiddle/type :query
   :fiddle/renderer (load-resource "ide/schema_renderer.cljs")
   :fiddle/links #{{:db/id :system-anchor-edit              ; XXX
                    :link/rel :hf/edit
                    :link/fiddle (db-attribute-edit $db)}
                   {:db/id :system-anchor-new               ; XXX
                    :link/rel :hf/new
                    :link/fiddle (db-attribute-edit $db)}}})
