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
      (dissoc :db/id)
      (util/update-existing :db/cardinality :db/ident)
      (util/update-existing :db/valueType :db/ident)
      (util/update-existing :db/unique :db/ident)))

(defn field-label [field param-ctx]
  (let [docstring (:field/doc field)
        field-prompt (util/fallback empty? (:field/prompt field) (-> param-ctx :attribute :db/ident str))
        display-mode @(:display-mode param-ctx)]
    [tooltip/hover-popover-managed
     {:label (case display-mode
               ; (auto-control maybe-field anchors props param-ctx)
               :user (if-not (empty? docstring) (markdown docstring #()))
               :xray [:pre (util/pprint-str (attribute-human (:attribute param-ctx)) 50)])}
     [:span {:class (case display-mode
                      :user (if-not (empty? docstring) "help")
                      :xray "help")} field-prompt]]
    #_[:div
       (let [is-ref? (coll? value)]
         (if is-ref?
           [tooltip/click-popover-managed
            {:body [code-editor/code-editor* (util/pprint-str value 100) nil {:readOnly true}]}
            [:a {:href "javascript:void 0;"} "§"]]))
       " "
       ]))