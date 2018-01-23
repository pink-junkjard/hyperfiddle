(ns hypercrud.ui.label
  (:require [cuerdas.core :as str]
            [hypercrud.compile.eval :refer [validate-user-code-str]]
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
  (->> (-> attr
           (util/update-existing :db/cardinality :db/ident)
           (util/update-existing :db/valueType :db/ident)
           (util/update-existing :db/unique :db/ident)
           (util/update-existing :attribute/renderer fqn->name)
           (select-keys [:attribute/renderer :db/valueType :db/cardinality :db/unique]))
       (reduce-kv (fn [acc k v] (conj acc v)) [])))

;(apply str (interpose " " (attribute-schema-human (:attribute ctx))))
; (eval/validate-user-code-str (-> ctx :attribute :db/doc))
#_{:label help-text :position :below-right}

(defn label [field ctx]
  (let [dbdoc (validate-user-code-str (-> ctx :attribute :db/doc))
        typedoc (apply str (interpose " " (attribute-schema-human (:attribute ctx))))
        help-md (str (if dbdoc (str dbdoc "\n\n"))          ; markdown needs double line-break
                     "`" typedoc "`")]
    ; There is always help-md rn but maybe there wont be and
    ; i like the if check on the styles
    [tooltip-thick (if help-md
                     [:div.docstring (markdown help-md)])
     [:label {:class (if help-md "help-available")}
      (-> ctx :attribute :db/ident str)
      (if help-md [:sup "†"])]]))
