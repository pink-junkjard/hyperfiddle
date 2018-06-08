(ns hypercrud.ui.label                                      ; hyperfiddle.ui/label
  (:require [contrib.reactive :as r]
            [contrib.string :refer [blank->nil]]
            [contrib.ui.tooltip :refer [tooltip-thick]]
            [cuerdas.core :as str]
            [contrib.ui :refer [markdown]]))


(defn fqn->name [s]
  (-> s
      (str/split "\n" 2)
      (first)

      ; both cljs and js work with cljs eval
      (as-> s (if (str/includes? s "/")
                (str/split s "/")
                (str/split s ".")))
      last))

(defn attribute-schema-human [attr]
  (-> attr ((juxt :db/ident
                  #(some-> % :attribute/renderer fqn->name)
                  #(some-> % :db/valueType :db/ident name)
                  #(some-> % :db/cardinality :db/ident name)
                  #(some-> % :db/isComponent (if :component) name)
                  #(some-> % :db/unique :db/ident name)))))

#_{:label help-text :position :below-right}

(defn auto-label [field ctx props]
  (let [dbdoc (some-> ctx :hypercrud.browser/fat-attribute (r/cursor [:db/doc]) deref blank->nil)
        typedoc (some->> ctx :hypercrud.browser/fat-attribute
                         (r/fmap attribute-schema-human)
                         deref (interpose " ") (apply str))
        help-md (str (if dbdoc (str dbdoc "\n\n"))          ; markdown needs double line-break
                     "`" typedoc "`")]
    [tooltip-thick (if help-md
                     [:div.docstring [markdown help-md]])
     [:label (:label field) (if help-md [:sup "†"])]]))
