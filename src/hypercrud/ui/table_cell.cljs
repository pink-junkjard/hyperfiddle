(ns hypercrud.ui.table-cell
  (:require [clojure.string :as string]
            [hypercrud.browser.links :as links]
            [hypercrud.form.option :as option]
            [hypercrud.ui.widget :as widget]))


(defn ellipsis
  ([s] (ellipsis 25 s))
  ([c s] (if (> c (count s))
           s
           (str (subs s 0 (- c 3)) "..."))))


(defn ref-one-component [entity {:keys [field] :as widget-args}]
  [:div
   (->> (get-in entity [(-> field :field/attribute :attribute/ident) :db/id])
        (pr-str))
   (widget/link-thing widget-args)
   ;todo render inline links
   ]
  #_(let [ident (-> field :field/attribute :attribute/ident)
          child-entity (get entity ident)]
      [:div
       (-> (option/label-prop field [child-entity])
           str
           ellipsis)]))


(defn ref-many [entity {:keys [field] :as widget-args}]
  [:div
   (->> (get entity (-> field :field/attribute :attribute/ident))
        (mapv :db/id)
        (pr-str)
        (ellipsis 15))
   (widget/link-thing widget-args)
   ;todo render inline links
   ]
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
