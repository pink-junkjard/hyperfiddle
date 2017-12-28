(ns hypercrud.ui.label
  (:require [cuerdas.core :as str]
            [hypercrud.browser.link :as link]
            [hypercrud.ui.tooltip :refer [tooltip-thick]]
            [hypercrud.util.core :as util]
            [hypercrud.compile.eval :as eval]
            [hypercrud.ui.control.markdown-rendered :refer [markdown]]))


(defn attribute-schema-human [attr]
  (->> (-> attr
           (util/update-existing :db/cardinality :db/ident)
           (util/update-existing :db/valueType :db/ident)
           (util/update-existing :db/unique :db/ident)
           (select-keys [:db/valueType :db/cardinality :db/unique]))
       (reduce-kv (fn [acc k v] (conj acc v)) [])))

;(apply str (interpose " " (attribute-schema-human (:attribute ctx))))
; (eval/validate-user-code-str (-> ctx :attribute :db/doc))
#_{:label help-text :position :below-right}

(defn label [field ctx]
  (let [help-text (-> ctx :attribute :db/doc)]
    [tooltip-thick (if help-text
                     [:div.docstring (markdown help-text)])
     [:label {:class (if help-text "help-available")}
      (-> ctx :attribute :db/ident str)
      (if help-text [:sup "†"])]]))
