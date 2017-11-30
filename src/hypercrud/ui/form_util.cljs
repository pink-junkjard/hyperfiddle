(ns hypercrud.ui.form-util
  (:require [cuerdas.core :as str]
            [hypercrud.ui.markdown :refer [markdown]]
            [hypercrud.ui.tooltip :as tooltip]
            [hypercrud.util.core :as util]))

(defn css-slugify [s]
  ; http://stackoverflow.com/a/449000/959627
  (-> s
      (str/replace ":" "-")
      (str/replace "/" "-")
      (str/replace " " "-")))

(defn build-props [field anchors ctx]
  ; why does this need the field - it needs the ident for readonly in "Edit Anchors"
  ; todo clean this interface up
  {:read-only ((get ctx :read-only) (:attribute ctx) ctx)})

(defn attribute-human [attr]
  (->> (-> attr
           (util/update-existing :db/cardinality :db/ident)
           (util/update-existing :db/valueType :db/ident)
           (util/update-existing :db/unique :db/ident)
           (select-keys [:db/valueType :db/cardinality :db/unique]))
       (reduce-kv (fn [acc k v] (conj acc v)) [])))

(defn field-label [field ctx]
  (let [label (-> ctx :attribute :db/ident str)
        help-text (apply str (interpose " " (attribute-human (:attribute ctx))))]
    [tooltip/fast-hover-tooltip-managed
     {:label help-text
      :position :below-right}
     [:span.help label]]))
