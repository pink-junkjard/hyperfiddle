(ns hypercrud.ui.table-cell
  (:require [clojure.string :as string]
            [hypercrud.browser.links :as links]
            [hypercrud.form.option :as option]
            [hypercrud.ui.select :as select]))


(defn ellipsis
  ([s] (ellipsis s 25))
  ([s c] (if (> c (count s))
           s
           (str (subs s 0 (- c 3)) "..."))))


(defn ref-one-component [entity {:keys [field navigate-cmp]}]
  (let [ident (-> field :field/attribute :attribute/ident)
        label-prop (:field/label-prop field)]
    [:div
     [navigate-cmp {:href (links/field-link (.-dbid field) (.-dbid entity))} "Edit"]
     " "
     (if-let [entity (get entity ident)]
       (-> entity
           (get label-prop)
           str
           ellipsis)
       "nil")]))


; this can be used sometimes, on the entity page, but not the query page
(defn ref-one [entity {:keys [expanded-cur field navigate-cmp] :as widget-args}]
  #_(if (option/has-holes? options)
      (ref-one-component entity form-id widget-args))
  (let [ident (-> field :field/attribute :attribute/ident)]
    (select/select*
      entity (assoc widget-args :expanded-cur (expanded-cur [ident]))
      (if (not (nil? (get entity ident)))
        (let [options (option/gimme-useful-options field)]
          (if-let [form (option/get-form options entity)]
            (let [href (links/entity-link (.-dbid form) (:db/id (get entity ident)))]
              [navigate-cmp {:class "edit" :href href} "Edit"])))))))


(defn ref-many [entity {:keys [field navigate-cmp]}]
  (let [ident (-> field :field/attribute :attribute/ident)
        label-prop (:field/label-prop field)]
    [:div
     [navigate-cmp {:href (links/field-link (.-dbid field) (.-dbid entity))} "Edit"]
     " "
     (->> (get entity ident)
          (map (fn [entity]
                 (if (not= nil entity)
                   (pr-str (get entity label-prop))
                   "nil")))
          (string/join ", "))]))


(defn other-many [entity {:keys [field navigate-cmp]}]
  (let [ident (-> field :field/attribute :attribute/ident)]
    [:div
     [navigate-cmp {:href (links/field-link (.-dbid field) (.-dbid entity))} "Edit"]
     " "
     (->> (get entity ident)
          (map pr-str)                                      ;todo account for many different types of values
          (string/join ", "))]))
