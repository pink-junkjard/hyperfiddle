(ns hypercrud.ui.label
  (:require [cuerdas.core :as str]
            [hypercrud.ui.control.markdown-rendered :refer [markdown]]
            [hypercrud.ui.tooltip :refer [tooltip-thick]]
            [contrib.reactive :as reactive]))


(defn fqn->name [s]
  ; both cljs and js work with cljs eval
  (-> (if (str/includes? s "/")
        (str/split s "/")
        (str/split s "."))
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

(defn label [field ctx]
  (let [dbdoc (let [dbdoc @(reactive/cursor (:hypercrud.browser/fat-attribute ctx) [:db/doc])]
                (when (and (string? dbdoc) (not (empty? dbdoc)))
                  dbdoc))
        typedoc (apply str (interpose " " @(reactive/track attribute-schema-human (:hypercrud.browser/fat-attribute ctx))))
        help-md (str (if dbdoc (str dbdoc "\n\n"))          ; markdown needs double line-break
                     "`" typedoc "`")]
    ; There is always help-md rn but maybe there wont be and
    ; i like the if check on the styles
    [tooltip-thick (if help-md
                     [:div.docstring [markdown help-md]])
     [:label {:class (if help-md "help-available")}
      ; common crash point; todo use field, attribute is explicitly NOT always defined
      (some-> (:hypercrud.browser/attribute ctx) name str)
      (if help-md [:sup "†"])]]))
