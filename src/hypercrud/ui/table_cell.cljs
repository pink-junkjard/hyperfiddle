(ns hypercrud.ui.table-cell
  (:require [clojure.string :as string]
            [hypercrud.browser.links :as links]
            [hypercrud.client.core :as hc]
            [hypercrud.form.option :as option]
            [hypercrud.ui.select :as select]))


(defn ellipsis
  ([s] (ellipsis s 25))
  ([s c] (if (> c (count s))
           s
           (str (subs s 0 (- c 3)) "..."))))


(defn ref-one-component [entity form-id {:keys [graph navigate-cmp] {:keys [ident options]} :field}]
  (let [label-prop (option/label-prop options)
        eid (get entity ident)]
    [:div
     [navigate-cmp {:href (links/field-link form-id (:db/id entity) ident)} "Edit"]
     " "
     (if eid
       (-> (hc/entity graph eid)
           (get label-prop)
           str
           ellipsis)
       "nil")]))


; this can be used sometimes, on the entity page, but not the query page
(defn ref-one [entity form-id {:keys [expanded-cur navigate-cmp queries] {:keys [ident options]} :field :as widget-args}]
  #_(if (option/has-holes? options queries)
    (ref-one-component entity form-id widget-args))
  (select/select*
    entity (assoc widget-args :expanded-cur (expanded-cur [ident]))
    (let [href (links/entity-link (option/get-form-id options entity) (get entity ident))]
      (if (not (nil? (get entity ident)))
        [navigate-cmp {:class "edit" :href href} "Edit"]))))


(defn ref-many [entity form-id {:keys [graph navigate-cmp] {:keys [ident options]} :field}]
  (let [label-prop (option/label-prop options)]
    [:div
     [navigate-cmp {:href (links/field-link form-id (:db/id entity) ident)} "Edit"]
     " "
     (->> (get entity ident)
          (map (fn [eid]
                 (if eid
                   (get (hc/entity graph eid) label-prop)
                   "nil")))
          (string/join ", "))]))


(defn other-many [entity form-id {:keys [navigate-cmp] {:keys [ident]} :field}]
  [:div
   [navigate-cmp {:href (links/field-link form-id (:db/id entity) ident)} "Edit"]
   " "
   (->> (get entity ident)
        (map pr-str)                                        ;todo account for many different types of values
        (string/join ", "))])
