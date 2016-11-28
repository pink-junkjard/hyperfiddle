(ns hypercrud.ui.table-cell
  (:require [clojure.string :as string]
            [hypercrud.browser.links :as links]
            [hypercrud.form.option :as option]))


(defn ellipsis
  ([s] (ellipsis s 25))
  ([s c] (if (> c (count s))
           s
           (str (subs s 0 (- c 3)) "..."))))


(defn ref-one-component [entity {:keys [field navigate-cmp]}]
  (let [ident (-> field :field/attribute :attribute/ident)]
    (if-let [child-entity (get entity ident)]
      [:div
       (let [{:keys [href]} (links/field-link (.-dbid field) (.-dbid entity))]
         [navigate-cmp {:href href} "Edit"])
       " "
       (-> (option/label-prop field [child-entity])
           str
           ellipsis)]
      [:div
       [:a {:href "#" :on-click #(js/alert "todo")} "Create"]])))


(defn ref-many [entity {:keys [field navigate-cmp]}]
  (let [ident (-> field :field/attribute :attribute/ident)]
    [:div
     (let [{:keys [href]} (links/field-link (.-dbid field) (.-dbid entity))]
       [navigate-cmp {:href href} "Edit"])
     " "
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
