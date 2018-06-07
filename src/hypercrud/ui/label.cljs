(ns hypercrud.ui.label                                      ; hyperfiddle.ui/label
  (:require [contrib.reactive :as r]
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
  (-> @attr ((juxt :db/ident
                   #(some-> % :attribute/renderer fqn->name)
                   #(some-> % :db/valueType :db/ident name)
                   #(some-> % :db/cardinality :db/ident name)
                   #(some-> % :db/isComponent (if :component) name)
                   #(some-> % :db/unique :db/ident name)))))

;(apply str (interpose " " (attribute-schema-human (:attribute ctx))))
; (eval/validate-user-code-str (-> ctx :attribute :db/doc))
#_{:label help-text :position :below-right}

(defn label [ctx]
  (let [dbdoc (let [dbdoc @(r/cursor (:hypercrud.browser/fat-attribute ctx) [:db/doc])]
                (when (and (string? dbdoc) (not (empty? dbdoc)))
                  dbdoc))
        typedoc (apply str (interpose " " @(r/track attribute-schema-human (:hypercrud.browser/fat-attribute ctx))))
        help-md (str (if dbdoc (str dbdoc "\n\n"))          ; markdown needs double line-break
                     "`" typedoc "`")]
    ; There is always help-md rn but maybe there wont be and
    ; i like the if check on the styles
    [tooltip-thick (if help-md
                     [:div.docstring [markdown help-md]])
     [:label
      ; common crash point; todo use field, attribute is explicitly NOT always defined
      (:label (:hypercrud.browser/field ctx))
      (if help-md [:sup "†"])]]))
