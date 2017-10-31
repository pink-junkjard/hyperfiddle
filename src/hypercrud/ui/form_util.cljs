(ns hypercrud.ui.form-util
  (:require [clojure.string :as string]
            [hypercrud.ui.markdown :refer [markdown]]
            [hypercrud.ui.tooltip :as tooltip]
            [hypercrud.util.core :as util]))

(defn css-slugify [s]
  ; http://stackoverflow.com/a/449000/959627
  (-> s
      (string/replace ":" "-")
      (string/replace "/" "-")
      (string/replace " " "-")))

(defn build-props [field anchors param-ctx]
  ; why does this need the field - it needs the ident for readonly in "Edit Anchors"
  ; todo clean this interface up
  {:read-only ((get param-ctx :read-only) (:attribute param-ctx) param-ctx)})

(defn attribute-human [attr]
  (-> attr
      (dissoc :db/id :db/doc)
      (util/update-existing :db/cardinality :db/ident)
      (util/update-existing :db/valueType :db/ident)
      (util/update-existing :db/unique :db/ident)))

(defn field-label [field param-ctx]
  (let [label (util/fallback empty? "" #_ (:field/prompt field) ; hook into i18n for this, can't store english in database
                                    (-> param-ctx :attribute :db/ident str))
        help-text (util/pprint-str (attribute-human (:attribute param-ctx)) 50)]
    [tooltip/hover-popover-managed
     {:label [:pre help-text]}
     [:span.help label]]
    #_[:div
       (let [is-ref? (coll? value)]
         (if is-ref?
           [tooltip/click-popover-managed
            {:body [code-editor/code-editor* (util/pprint-str value 100) nil {:readOnly true}]}
            [:a {:href "javascript:void 0;"} "§"]]))
       " "
       ]))