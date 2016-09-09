(ns hypercrud.ui.form-util)


(defn field->option-query [field]
  ; todo account for holes
  (let [{{[query args] :query/value} :field/query} field
        query-name (hash query)]
    {query-name [query [] '[*]]}))


(defn fieldref->form [forms field]
  (get forms (get field :field/form)))


(declare form-pull-exp)
(declare form-queries)


(defn field-pull-exp-entry [forms expanded-forms {:keys [:attribute/ident :attribute/isComponent] :as field}]
  (let [new-expanded-forms (get expanded-forms ident)]
    (if (or new-expanded-forms isComponent)
      ; components render expanded automatically
      ; so if it is expanded or is a component, pull another level deeper
      (let [form (fieldref->form forms field)]
        {ident (form-pull-exp forms form new-expanded-forms)})

      ; otherwise just add the attribute to the list
      ident)))


(defn form-pull-exp "generate the pull expression recursively for all expanded forms"
  [forms form expanded-forms]
  (concat
    [:db/id]
    (map (partial field-pull-exp-entry forms expanded-forms) form)))


(defn field-queries [forms expanded-forms {:keys [:attribute/ident :attribute/valueType :attribute/isComponent] :as field}]
  (merge
    (let [is-ref (= valueType :ref)]
      ; if we are a ref we ALWAYS need the query from the field options
      ; EXCEPT when we are component, in which case no options are rendered, just a form, handled below
      (if (and is-ref (not isComponent)) (field->option-query field)))

    (let [new-expanded-forms (get expanded-forms ident)]
      ; components render expanded automatically
      ; so if it is expanded or is a component, get the queries for another level deeper
      (if (or new-expanded-forms isComponent)
        (let [form (fieldref->form forms field)]
          (form-queries forms form new-expanded-forms))))))


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
