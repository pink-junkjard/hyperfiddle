(ns hypercrud.ui.form
  (:require [hypercrud.ui.auto-control :refer [auto-control]]
            [hypercrud.client.core :as hc]
            [hypercrud.client.tx :as tx-util]))


(defn field [{:keys [:attribute/ident :field/prompt] :as fieldinfo} graph entity forms expanded-cur local-transact! tempid!]
  (let [value (get entity ident)
        change! (fn [retracts adds]
                  (local-transact!
                    (vec (concat (map (fn [val] [:db/retract (:db/id entity) ident val]) retracts)
                                 (map (fn [val] [:db/add (:db/id entity) ident val]) adds)))))]
    [:div.field
     (if prompt [:label prompt])
     [auto-control fieldinfo graph forms value expanded-cur change! local-transact! tempid!]]))


(defn form [graph eid form-id forms expanded-cur local-transact! tempid!]
  (let [entity (hc/entity graph eid)]
    [:div.form
     (map (fn [{:keys [:attribute/ident] :as fieldinfo}]
            ^{:key ident}
            [field fieldinfo graph entity forms expanded-cur local-transact! tempid!])
          (get forms form-id))]))


(defn find' [pred coll]
  (first (filter pred coll)))


(defn form->options-queries [form]                          ; todo account for holes
  (->> form
       (filter (fn [{:keys [:attribute/valueType]}] (= valueType :ref)))
       (map (fn [{{{[query args] :query/value} :option/query} :field/options}]
              (let [query-name (hash query)]
                [query-name [query [] '[*]]])))             ;:db/id label-prop
       (into {})))


(defn fieldname->field [form fieldname]
  (find' #(= (:attribute/ident %) fieldname) form))


(defn fieldref->form [forms field]
  (get forms (get-in field [:field/options :option/form])))


(defn expanded-form-pull-exp "generate the pull expression recursively for all expanded forms"
  [forms form expanded-forms]
  (concat
    [:db/id]

    (->> (map :attribute/ident form)
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
    (if-not (tx-util/tempid? eid)
      {::query ['[:find [?eid ...] :in $ ?eid :where [?eid]]
                [eid]
                (expanded-form-pull-exp forms root-form expanded-forms)]})
    (expanded-form-queries forms root-form expanded-forms)))
