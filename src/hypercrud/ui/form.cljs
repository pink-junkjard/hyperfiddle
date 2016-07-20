(ns hypercrud.ui.form
  (:require [hypercrud.ui.auto-control :refer [auto-control]]
            [hypercrud.client.core :as hc]))


(defn cj-form-field [{:keys [name prompt] :as fieldinfo}
                     graph metatype forms value expanded-cur
                     change! transact! tempid!]
  [:div.cj-field
   [:label prompt]
   [auto-control fieldinfo graph metatype forms value expanded-cur change! transact! tempid!]])


(defn cj-form [graph eid metatype forms expanded-cur local-transact! tempid!]
  (let [entity (hc/entity graph eid)]
    [:div.cj-form
     (map (fn [{:keys [name] :as fieldinfo}]
            (let [value (get entity name)
                  change! (fn [& op-vals] (local-transact! (mapv (fn [[op val]] [op eid name val]) op-vals)))]
              ^{:key name}
              [cj-form-field fieldinfo graph metatype forms value expanded-cur change! local-transact! tempid!]))
          (metatype forms))]))


(defn find' [pred coll]
  (first (filter pred coll)))


(defn form->options-queries [form]
  (->> form
       (filter (fn [{:keys [datatype]}] (= datatype :ref)))
       (map (fn [{{q :query} :options}]
              (let [query-name (hash q)]
                [query-name [q [] '[*]]])))
       (into {})))

(defn fieldname->field [form fieldname]
  (find' #(= (:name %) fieldname) form))


(defn fieldref->form [forms field]
  ((get-in field [:options :form]) forms))


(defn expanded-form-queries "get the form options, recursively, for all expanded forms"
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


(defn cj-form-dependencies [eid expanded-forms root-form forms]
  (merge
    {::query ['[:find [?eid ...] :in $ ?eid :where [?eid]]
              [(js/parseInt eid 10)]
              (concat [:db/id] (map :name root-form))]}     ;; go deep based on expansions
    (expanded-form-queries forms root-form expanded-forms)))
