(ns hypercrud.form.util
  (:require [hypercrud.form.option :as option]))


(defn options->form [forms field-options]
  (get forms (option/get-form-id field-options "todo")))


(defn- deep-merge "recursively merges maps. if vals are not maps, will throw (which is a very particular
case that works for expanded-forms)"
  [& vals]
  (apply merge-with deep-merge vals))


(declare form-pull-exp)
(declare form-queries)


(defn field-pull-exp-entry [forms expanded-forms {:keys [:ident :cardinality :isComponent :options] :as field}]
  (let [new-expanded-forms (get expanded-forms ident)]
    (if (or new-expanded-forms isComponent)
      ; components render expanded automatically
      ; so if it is expanded or is a component, pull another level deeper
      (let [form (options->form forms options)]
        {ident (form-pull-exp forms form (condp = cardinality
                                           :db.cardinality/one new-expanded-forms
                                           :db.cardinality/many (apply deep-merge (vals new-expanded-forms))))})

      ; otherwise just add the attribute to the list
      ident)))


(defn form-pull-exp "generate the pull expression recursively for all expanded forms"
  [forms form expanded-forms]
  (concat
    [:db/id]
    (map (partial field-pull-exp-entry forms expanded-forms) form)))


(defn field-queries [forms expanded-forms {:keys [:ident :cardinality :valueType :isComponent :options] :as field}]
  (merge
    (let [is-ref (= valueType :ref)]
      ; if we are a ref we ALWAYS need the query from the field options
      ; EXCEPT when we are component, in which case no options are rendered, just a form, handled below
      (if (and is-ref (not isComponent)) (option/get-query options)))

    (let [new-expanded-forms (get expanded-forms ident)]
      ; components render expanded automatically
      ; so if it is expanded or is a component, get the queries for another level deeper
      (if (or new-expanded-forms isComponent)
        (let [form (options->form forms options)]
          (form-queries forms form (condp = cardinality
                                     :db.cardinality/one new-expanded-forms
                                     :db.cardinality/many (apply deep-merge (vals new-expanded-forms)))))))))


(defn form-queries "get the form options recursively for all expanded forms"
  [forms form expanded-forms]
  (apply merge
         (map (partial field-queries forms expanded-forms)
              form)))


(defn query [forms form-id expanded-forms {:keys [:query-name :query :params] :as q-and-params}]
  (let [form (get forms form-id)
        queries-map (form-queries forms form expanded-forms)]
    (if q-and-params
      (let [value [query params (form-pull-exp forms form expanded-forms)]]
        (assoc queries-map (or query-name (hash value)) value))
      queries-map)))
