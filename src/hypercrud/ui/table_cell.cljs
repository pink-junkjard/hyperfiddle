(ns hypercrud.ui.table-cell
  (:require [clojure.string :as string]
            [hypercrud.ui.widget :as widget]))


(defn ellipsis
  ([s] (ellipsis 25 s))
  ([c s] (if (> c (count s))
           s
           (str (subs s 0 (- c 3)) "..."))))


(defn ref-one-component [entity field links param-ctx]
  [:div
   #_(->> (get-in entity [(-> field :field/attribute :attribute/ident) :db/id])
        (pr-str))
   (widget/link-thing field links param-ctx)
   (widget/render-inline-links field links param-ctx)])


(defn ref-many [entity field links param-ctx]
  [:div
   #_(->> (get entity (-> field :field/attribute :attribute/ident))
        (mapv :db/id)
        (pr-str)
        (ellipsis 15))
   (widget/link-thing field links param-ctx)
   (widget/render-inline-links field links param-ctx)])


(defn other-many [entity field links param-ctx]
  (let [ident (-> field :field/attribute :attribute/ident)]
    [:div
     [:button {:on-click #(js/alert "todo")} "Edit"]
     #_(let [{:keys [href]} (links/field-link (.-dbid field) (.-dbid entity))]
       [navigate-cmp {:href href} "Edit"])
     " "
     (->> (get entity ident)
          (map pr-str)                                      ;todo account for many different types of values
          (string/join ", "))]))
