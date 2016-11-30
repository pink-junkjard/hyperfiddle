(ns hypercrud.ui.table-cell
  (:require [clojure.string :as string]
            [hypercrud.browser.links :as links]
            [hypercrud.form.option :as option]
            [hypercrud.ui.widget :as widget]))


(defn ellipsis
  ([s] (ellipsis s 25))
  ([s c] (if (> c (count s))
           s
           (str (subs s 0 (- c 3)) "..."))))


(defn ref-one-component [entity {:keys [field] :as widget-args}]
  (widget/link-thing widget-args)
  #_(let [ident (-> field :field/attribute :attribute/ident)
          child-entity (get entity ident)]
      [:div
       (-> (option/label-prop field [child-entity])
           str
           ellipsis)]))


(defn ref-many [entity {:keys [field]}]
  [:div "todo"]
  #_(let [ident (-> field :field/attribute :attribute/ident)]
      [:div
       (->> (get entity ident)
            (map (fn [entity]
                   (if (not= nil entity)
                     (option/label-prop field [entity])
                     "nil")))
            (string/join ", "))]))


(defn other-many [entity {:keys [field navigate-cmp]}]
  (let [ident (-> field :field/attribute :attribute/ident)]
    [:div
     (let [{:keys [href]} (links/field-link (.-dbid field) (.-dbid entity))]
       [navigate-cmp {:href href} "Edit"])
     " "
     (->> (get entity ident)
          (map pr-str)                                      ;todo account for many different types of values
          (string/join ", "))]))
