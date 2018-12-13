(ns hyperfiddle.ide.system-fiddle
  (:require
    [contrib.template :refer [load-resource]]
    [contrib.try$ :refer [try-either]]
    [cuerdas.core :as str]
    [hyperfiddle.ide.fiddles.errors :as errors]
    [hyperfiddle.ide.fiddles.schema :as schema]))


(defn system-fiddle? [fiddle-ident]
  (and (keyword? fiddle-ident)                              ; why long here wut?
       (namespace fiddle-ident)
       ; hyperfiddle.ide is real
       (or (-> (namespace fiddle-ident) (str/starts-with? "hyperfiddle.system"))
           (-> (namespace fiddle-ident) (str/starts-with? "hyperfiddle.schema")))))

; these need to be thick/hydrated params bc we are manufacturing a pulled tree here.

; TODO: manufacture pull from parent pull
(defn console-self [dbname]
  {:fiddle/ident (keyword "hyperfiddle.system" (str "self-" dbname))
   :fiddle/type :entity
   :fiddle/pull-database (str dbname)
   :fiddle/renderer (load-resource "ide/console_renderer.cljs")})

(defn console-new [dbname]
  {:fiddle/ident (keyword "hyperfiddle.system" (str "new-" dbname))
   :fiddle/type :entity
   :fiddle/pull-database (str dbname)
   :fiddle/renderer (load-resource "ide/console_renderer.cljs")})

(defn console-affix [dbname]
  {:fiddle/ident (keyword "hyperfiddle.system" (str "affix-" dbname))
   :fiddle/type :entity
   :fiddle/pull-database (str dbname)
   :fiddle/renderer (load-resource "ide/console_renderer.cljs")})

(defn hydrate-system-fiddle [ident]
  (try-either                                               ; catch all the pre assertions
    (let [name' (name ident)]
      (case (namespace ident)
        "hyperfiddle.system" (cond
                               (= name' "unauthorized") errors/unauthorized
                               (str/starts-with? name' "self-") (console-self (str/strip-prefix name' "self-"))
                               (str/starts-with? name' "new-") (console-new (str/strip-prefix name' "new-"))
                               (str/starts-with? name' "affix-") (console-affix (str/strip-prefix name' "affix-")))
        "hyperfiddle.system.route" (cond
                                     (= name' "decoding-error") errors/decoding-error
                                     (= name' "home-route-error") errors/home-route-error
                                     (= name' "not-found") errors/not-found)
        "hyperfiddle.schema" (schema/schema name')
        "hyperfiddle.schema.db-cardinality-options" (schema/db-cardinality-options name')
        "hyperfiddle.schema.db-unique-options" (schema/db-unique-options name')
        "hyperfiddle.schema.db-valueType-options" (schema/db-valueType-options name')
        "hyperfiddle.schema.db-attribute-edit" (schema/db-attribute-edit name')))))
