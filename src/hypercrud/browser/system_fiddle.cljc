(ns hypercrud.browser.system-fiddle
  (:require [cuerdas.core :as str]
            [contrib.try :refer [try-either]]
            [hyperfiddle.ide.fiddles.schema :as schema]
            [hyperfiddle.ide.fiddles.not-found :as not-found]
            [hypercrud.types.Entity :refer [->Entity]]))


(defn system-fiddle? [fiddle-ident]
  (and (keyword? fiddle-ident)                              ; why long here wut?
       (namespace fiddle-ident)
       ; hyperfiddle.ide is real
       (or (-> (namespace fiddle-ident) (str/starts-with? "hyperfiddle.system"))
           (-> (namespace fiddle-ident) (str/starts-with? "hyperfiddle.schema")))))

; these need to be thick/hydrated params bc we are manufacturing a pulled tree here.

(defn fiddle-system-edit [dbname]
  {:fiddle/ident (keyword "hyperfiddle.system" (str "edit-" dbname))
   :fiddle/type :entity
   :fiddle/pull-database dbname})

(def fiddle-blank-system-remove
  {:fiddle/ident :hyperfiddle.system/remove
   :fiddle/type :blank
   :fiddle/renderer (str '[:p "Retract entity?"])})

(defn hydrate-system-fiddle [ident]
  (try-either                                               ; catch all the pre assertions
    (let [name' (name ident)]
      (case (namespace ident)
        "hyperfiddle.system" (cond
                               (= name' "remove") fiddle-blank-system-remove
                               (= name' "not-found") not-found/not-found
                               (str/starts-with? name' "edit-") (fiddle-system-edit (str/strip-prefix name' "edit-")))
        "hyperfiddle.schema" (schema/schema name')
        "hyperfiddle.schema.db-cardinality-options" (schema/db-cardinality-options name')
        "hyperfiddle.schema.db-unique-options" (schema/db-unique-options name')
        "hyperfiddle.schema.db-valueType-options" (schema/db-valueType-options name')
        "hyperfiddle.schema.db-attribute-edit" (schema/db-attribute-edit name')))))
