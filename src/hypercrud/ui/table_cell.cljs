(ns hypercrud.ui.table-cell
  (:require [clojure.string :as string]
            [hypercrud.browser.base-64-url-safe :as base64]
            [hypercrud.client.core :as hc]
            [hypercrud.form.option :as option]
            [hypercrud.ui.select :as select]))


(defn ellipsis
  ([s] (ellipsis s 25))
  ([s c] (if (> c (count s))
           s
           (str (subs s 0 (- c 3)) "..."))))


(defn ref-one [entity form-id {:keys [expanded-cur navigate-cmp] {:keys [ident options]} :field :as widget-args}]
  (select/select*
    entity (assoc widget-args :expanded-cur (expanded-cur [ident]))
    (let [href (str (option/get-form-id options entity) "/entity/" (get entity ident))]
      (if (not (nil? (get entity ident)))
        [navigate-cmp {:class "edit" :href href} "Edit"]))))


(defn ref-one-component [entity form-id {:keys [graph navigate-cmp] {:keys [ident options]} :field}]
  (let [label-prop (option/label-prop options)
        eid (get entity ident)]
    [navigate-cmp {:href (str form-id "/entity/" (:db/id entity) "/" (base64/encode ident))}
     (if eid
       (ellipsis (get (hc/entity graph eid) label-prop))
       "nil")]))


(defn ref-many [entity form-id {:keys [graph navigate-cmp] {:keys [ident options]} :field}]
  (let [label-prop (option/label-prop options)]
    [navigate-cmp {:href (str form-id "/entity/" (:db/id entity) "/" (base64/encode ident))}
     (->> (get entity ident)
          (map (fn [eid]
                 (if eid
                   (get (hc/entity graph eid) label-prop)
                   "nil")))
          (string/join ", "))]))


(defn other-many [entity form-id {:keys [navigate-cmp] {:keys [ident]} :field}]
  [navigate-cmp {:href (str form-id "/entity/" (:db/id entity) "/" (base64/encode ident))}
   (->> (get entity ident)
        (map pr-str)                                        ;todo account for many different types of values
        (string/join ", "))])
