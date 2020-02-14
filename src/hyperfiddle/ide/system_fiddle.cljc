(ns hyperfiddle.ide.system-fiddle
  (:require
    [clojure.set :as set]
    [contrib.try$ :refer [try-either]]))


(defn db-cardinality-options [ide-dbname]
  {:fiddle/ident (keyword "hyperfiddle.ide.schema" (str "options.cardinality" ide-dbname))
   :fiddle/type :query
   :fiddle/query (let [$db (symbol ide-dbname)]
                   (str [:in $db :find (list 'pull $db '?e [:db/ident]) :where
                         [$db '?e :db/ident '?ident]
                         '[(namespace ?ident) ?ns]
                         '[(= ?ns "db.cardinality")]]))})

(defn db-unique-options [ide-dbname]
  {:fiddle/ident (keyword "hyperfiddle.ide.schema" (str "options.unique" ide-dbname))
   :fiddle/type :query
   :fiddle/query (let [$db (symbol ide-dbname)]
                   (str [:in $db :find (list 'pull $db '?e [:db/ident]) :where
                         [$db '?e :db/ident '?ident]
                         '[(namespace ?ident) ?ns]
                         '[(= ?ns "db.unique")]]))})

(defn db-valueType-options [ide-dbname]
  {:fiddle/ident (keyword "hyperfiddle.ide.schema" (str "options.valueType" ide-dbname))
   :fiddle/type :query
   :fiddle/query (let [$db (symbol ide-dbname)]
                   (str [:in $db :find (list 'pull $db '?valueType [:db/ident]) :where
                         [$db '?db-part :db.install/valueType '?valueType]
                         [$db '?db-part :db/ident :db.part/db]]))})

(defn attribute-editor [ide-dbname]
  (let [ident (keyword "hyperfiddle.ide.schema" (str "editor.attribute" ide-dbname))]
    {:fiddle/ident ident
     :fiddle/type :entity
     :fiddle/pull (str [:db/id                              ; for smart-identity tempid stability
                        :db/ident
                        {:db/valueType [:db/ident]}
                        {:db/cardinality [:db/ident]}
                        :db/doc
                        {:db/unique [:db/ident]}
                        :db/isComponent
                        :db/fulltext])
     :fiddle/pull-database ide-dbname
     :fiddle/renderer "hyperfiddle.ide.fiddles.schema-attribute/renderer"
     :fiddle/links #{{:link/path (str ident)
                      :link/fiddle (db-cardinality-options ide-dbname)
                      :link/class #{:hf/iframe :cardinality-options}}
                     {:link/path (str ident)
                      :link/fiddle (db-unique-options ide-dbname)
                      :link/class #{:hf/iframe :unique-options}}
                     {:link/path (str ident)
                      :link/fiddle (db-valueType-options ide-dbname)
                      :link/class #{:hf/iframe :valueType-options}}}}))

(defn attribute [user-dbname user-dbname->ide]
  (let [ident (keyword "hyperfiddle.ide.schema" (str "attribute" user-dbname))
        ide-dbname (user-dbname->ide user-dbname)]
    {:fiddle/ident ident
     :fiddle/links #{{:link/class #{:hf/iframe}
                      :link/fiddle {:fiddle/ident :hyperfiddle/topnav
                                    :fiddle/type :blank}
                      :link/path (str ident)}
                     {:link/class #{:hf/iframe}
                      :link/fiddle (attribute-editor ide-dbname)
                      :link/formula "(constantly (first (:hyperfiddle.route/datomic-args @(:hypercrud.browser/route ctx))))"
                      :link/path (str ident)}}
     :fiddle/renderer (str "[:<>\n [hyperfiddle.ui/ui-from-link\n  (hyperfiddle.data/select ctx :hyperfiddle/topnav) ctx {}] \n [hyperfiddle.ui/ui-from-link\n  (hyperfiddle.data/select ctx :hyperfiddle.ide.schema/editor.attribute" ide-dbname ") ctx {}]]")
     :fiddle/css ".-hyperfiddle-ide-schema-editor-attribute {\n  flex: 1 1;\n  overflow: auto;\n}"}))

(defn schema-editor [ide-dbname user-dbname->ide]
  {:fiddle/ident (keyword "hyperfiddle.ide.schema" (str "editor" ide-dbname))
   :fiddle/type :query
   :fiddle/query (let [$db (symbol ide-dbname)]
                   (str [:in $db :find (list 'pull $db '?attr
                                             [:db/id
                                              :db/ident
                                              {:db/valueType [:db/ident]}
                                              {:db/cardinality [:db/ident]}
                                              {:db/unique [:db/ident]}
                                              :db/isComponent :db/fulltext :db/doc])
                         :where [$db :db.part/db :db.install/attribute '?attr]
                         ; binding for ui
                         [$db '?attr :db/ident '?ident]]))
   :fiddle/renderer "hyperfiddle.ide.fiddles.schema-editor/renderer"
   :fiddle/links #{{:db/id :hyperfiddle.ide.schema.link/edit-attribute
                    :link/path (str ide-dbname " :db/ident")
                    :link/fiddle (attribute ((set/map-invert user-dbname->ide) ide-dbname) user-dbname->ide)
                    :link/formula "identity"}
                   {:db/id :hyperfiddle.ide.schema.link/new-attribute
                    :link/path (str ide-dbname " :db/ident")
                    :link/class #{:hf/new}
                    :link/fiddle (attribute-editor ide-dbname)}}})

(defn schema [user-dbname user-dbname->ide]
  (let [ident (keyword "hyperfiddle.ide.schema" user-dbname)
        ide-dbname (user-dbname->ide user-dbname)]
    {:fiddle/ident ident
     :fiddle/type :blank
     :fiddle/links #{{:link/class #{:hf/iframe}
                      :link/fiddle (schema-editor ide-dbname user-dbname->ide)
                      :link/path (str ident)}
                     {:link/class #{:hf/iframe}
                      :link/fiddle {:fiddle/ident :hyperfiddle/topnav
                                    :fiddle/type :blank}
                      :link/path (str ident)}}
     :fiddle/renderer (str "[:<>\n [hyperfiddle.ui/ui-from-link\n  (hyperfiddle.data/select ctx :hyperfiddle/topnav) ctx {}] \n [hyperfiddle.ui/ui-from-link\n  (hyperfiddle.data/select ctx :hyperfiddle.ide.schema/editor" ide-dbname ") ctx {}]]")
     :fiddle/css ".-hyperfiddle-ide-schema-editor {\n  flex: 1 1;\n  overflow: auto;\n}"}))

(defn hydrate [fiddle-ident user-dbname->ide]
  (try-either
    (let [[_ fiddle-name dbname] (re-find #"([^\$]*)(\$.*)" (name fiddle-ident))]
      (case fiddle-name
        "" (schema dbname user-dbname->ide)
        "editor" (schema-editor dbname user-dbname->ide)
        "attribute" (attribute dbname user-dbname->ide)
        "editor.attribute" (attribute-editor dbname)
        "options.cardinality" (db-cardinality-options dbname)
        "options.unique" (db-unique-options dbname)
        "options.valueType" (db-valueType-options dbname)))))