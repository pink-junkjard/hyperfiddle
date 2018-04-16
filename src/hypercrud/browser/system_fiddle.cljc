(ns hypercrud.browser.system-fiddle
  (:require [clojure.string :as str]
            [contrib.try :refer [try-either]]
            [contrib.macros :refer [str-and-code]]))


(defn system-fiddle? [fiddle-ident]
  (and (keyword? fiddle-ident)                              ; why long here wut?
       (namespace fiddle-ident)
       ; hyperfiddle.ide is real
       (or (-> (namespace fiddle-ident) (str/starts-with? "hyperfiddle.system"))
           (-> (namespace fiddle-ident) (str/starts-with? "hyperfiddle.schema")))))

; these need to be thick/hydrated params bc we are manufacturing a pulled tree here.

(def fiddle-system-edit
  {:fiddle/ident :hyperfiddle.system/edit
   :fiddle/type :entity})

(def fiddle-blank-system-remove
  {:fiddle/ident :hyperfiddle.system/remove
   :fiddle/type :blank
   :fiddle/renderer (str-and-code
                      (fn [ctx]
                        [:p "Retract entity?"]))})


(defn hydrate-system-fiddle [ident]
  (try-either                                               ; catch all the pre assertions
    (cond
      (= ident :hyperfiddle.system/edit) fiddle-system-edit
      (= ident :hyperfiddle.system/remove) fiddle-blank-system-remove
      :else (let [$db (name ident)]
              (condp = (namespace ident)
                ; ide cannot be imported from browser ns, creates circular deps
                "hyperfiddle.schema" (hyperfiddle.ide.fiddles.schema/schema $db)
                "hyperfiddle.schema.db-cardinality-options" (hyperfiddle.ide.fiddles.schema/db-cardinality-options $db)
                "hyperfiddle.schema.db-unique-options" (hyperfiddle.ide.fiddles.schema/db-unique-options $db)
                "hyperfiddle.schema.db-valueType-options" (hyperfiddle.ide.fiddles.schema/db-valueType-options $db)
                "hyperfiddle.schema.db-attribute-edit" (hyperfiddle.ide.fiddles.schema/db-attribute-edit $db))))))
