(ns hypercrud.ui.label
  (:require [hypercrud.browser.anchor :as link]
            [hypercrud.ui.tooltip :as tooltip]
            [hypercrud.util.core :as util]))


(defn attribute-schema-human [attr]
  (->> (-> attr
           (util/update-existing :db/cardinality :db/ident)
           (util/update-existing :db/valueType :db/ident)
           (util/update-existing :db/unique :db/ident)
           (select-keys [:db/valueType :db/cardinality :db/unique]))
       (reduce-kv (fn [acc k v] (conj acc v)) [])))

(defn label-inner [field ctx]
  (let [label (-> ctx :attribute :db/ident str)
        help-text (apply str (interpose " " (attribute-schema-human (:attribute ctx))))]
    [tooltip/fast-hover-tooltip-managed
     {:label help-text
      :position :below-right}
     [:span.help label]]))
