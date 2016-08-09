(ns hypercrud.ui.form
  (:require [hypercrud.ui.auto-control :refer [auto-control]]
            [hypercrud.client.core :as hc]))


(defn field [{:keys [name prompt] :as fieldinfo} graph entity forms expanded-cur local-transact! tempid!]
  (let [value (get entity name)
        change! (fn [& op-vals] (local-transact! (mapv (fn [[op val]] [op (:db/id entity) name val]) op-vals)))]
    [:div.field
     (if prompt [:label prompt])
     [auto-control fieldinfo graph forms value expanded-cur change! local-transact! tempid!]]))


(defn form [graph eid metatype forms expanded-cur local-transact! tempid!]
  (let [entity (hc/entity graph eid)]
    [:div.form
     (map (fn [{:keys [name] :as fieldinfo}]
            ^{:key name}
            [field fieldinfo graph entity forms expanded-cur local-transact! tempid!])
          (metatype forms))]))


(defn find' [pred coll]
  (first (filter pred coll)))


(defn form->options-queries [form]
  (->> form
       (filter (fn [{:keys [datatype]}] (= datatype :ref)))
       (map (fn [{{:keys [query label-prop]} :options}]
              (let [query-name (hash query)]
                [query-name [query [] '[*]]])))             ;:db/id label-prop
       (into {})))


(defn fieldname->field [form fieldname]
  (find' #(= (:name %) fieldname) form))


(defn fieldref->form [forms field]
  ((get-in field [:options :form]) forms))


(defn expanded-form-pull-exp "generate the pull expression recursively for all expanded forms"
  [forms form expanded-forms]
  (concat
    [:db/id]

    (->> (map :name form)
         (filter #((complement contains?) expanded-forms %)))

    (map (fn [[fieldname expanded-forms]]
           (let [field (fieldname->field form fieldname)
                 form (fieldref->form forms field)]
             {fieldname (expanded-form-pull-exp forms form expanded-forms)}))
         expanded-forms)))


(defn expanded-form-queries "get the form options recursively for all expanded forms"
  [forms form expanded-forms]
  (merge
    (form->options-queries form)
    (apply merge (map (fn [[fieldname expanded-forms]]
                        (let [field (fieldname->field form fieldname)
                              form (fieldref->form forms field)]
                          (expanded-form-queries forms form expanded-forms)))
                      expanded-forms))))

(comment
  "scrap for expanded-form-queries"
  (->> (tree-seq map? vals expanded-forms)
       (map #(form->options-queries (fieldref->form forms (fieldname->field root-form %))))
       (apply merge)))


(defn query [eid expanded-forms root-form forms]            ;bad abstraction/not an abstraction
  (merge
    {::query ['[:find [?eid ...] :in $ ?eid :where [?eid]]
              [eid]
              (expanded-form-pull-exp forms root-form expanded-forms)]}
    (expanded-form-queries forms root-form expanded-forms)))
