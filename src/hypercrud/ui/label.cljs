(ns hypercrud.ui.label
  (:require [cuerdas.core :as str]
            [hypercrud.browser.link :as link]
            [hypercrud.ui.tooltip :as tooltip]
            [hypercrud.util.core :as util]
            [hypercrud.compile.eval :as eval]))


(defn attribute-schema-human [attr]
  (->> (-> attr
           (util/update-existing :db/cardinality :db/ident)
           (util/update-existing :db/valueType :db/ident)
           (util/update-existing :db/unique :db/ident)
           (select-keys [:db/valueType :db/cardinality :db/unique]))
       (reduce-kv (fn [acc k v] (conj acc v)) [])))

(defn label [field ctx]
  (let [label (-> ctx :attribute :db/ident str)
        ;help-text (apply str (interpose " " (attribute-schema-human (:attribute ctx))))
        help-text (eval/validate-user-code-str (-> ctx :attribute :db/doc))]
    [:label {:class (if help-text "help-available")}
     (if help-text
       [tooltip/fast-hover-tooltip-managed {:label help-text :position :below-right} label]
       label)]))
