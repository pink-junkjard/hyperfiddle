(ns hypercrud.ui.label
  (:require [cuerdas.core :as str]
            [hypercrud.ui.control.markdown-rendered :refer [markdown]]
            [hypercrud.ui.tooltip :refer [tooltip-thick]]
            [hypercrud.util.core :as util]))


(defn fqn->name [s]
  ; both cljs and js work with cljs eval
  (-> (if (str/includes? s "/")
        (str/split s "/")
        (str/split s "."))
      last))

(defn attribute-schema-human [attr]
  (-> attr ((juxt :db/ident
                  #(some-> % :attribute/renderer fqn->name)
                  #(some-> % :db/valueType :db/ident name)
                  #(some-> % :db/cardinality :db/ident name)
                  #(some-> % :db/isComponent (if :component) name)
                  #(some-> % :db/unique :db/ident name)))))

;(apply str (interpose " " (attribute-schema-human (:attribute ctx))))
; (eval/validate-user-code-str (-> ctx :attribute :db/doc))
#_{:label help-text :position :below-right}

(defn label [field ctx]
  (let [dbdoc (let [dbdoc (-> ctx :attribute :db/doc)]
                (when (and (string? dbdoc) (not (empty? dbdoc)))
                  dbdoc))
        typedoc (apply str (interpose " " (attribute-schema-human (:attribute ctx))))
        help-md (str (if dbdoc (str dbdoc "\n\n"))          ; markdown needs double line-break
                     "`" typedoc "`")]
    ; There is always help-md rn but maybe there wont be and
    ; i like the if check on the styles
    [tooltip-thick (if help-md
                     [:div.docstring [markdown help-md]])
     [:label {:class (if help-md "help-available")}
      (some-> ctx :attribute :db/ident name str)            ; common crash point, though this should be defined
      (if help-md [:sup "†"])]]))
