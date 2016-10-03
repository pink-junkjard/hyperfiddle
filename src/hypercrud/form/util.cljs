(ns hypercrud.form.util
  (:require [hypercrud.form.option :as option]))


(defn options->form [forms field-options]
  (get forms (option/get-form-id field-options "todo")))


(defn- deep-merge "recursively merges maps. if vals are not maps, will throw (which is a very particular
case that works for expanded-forms)"
  [& vals]
  (apply merge-with deep-merge vals))


(declare form-pull-exp)
(declare form-option-queries)


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
    (map #(field-pull-exp-entry forms expanded-forms %) form)))


(defn field-queries [forms queries expanded-forms param-ctx
                     {:keys [:ident :cardinality :valueType :isComponent :options] :as field}]
  (merge
    (let [is-ref (= valueType :ref)]
      ; if we are a ref we ALWAYS need the query from the field options
      ; EXCEPT when we are component, in which case no options are rendered, just a form, handled below
      (if (and is-ref (not isComponent)) (option/get-query options queries param-ctx)))

    (let [new-expanded-forms (get expanded-forms ident)]
      ; components render expanded automatically
      ; so if it is expanded or is a component, get the queries for another level deeper
      (if (or new-expanded-forms (= cardinality :db.cardinality/many) isComponent)
        (let [form (options->form forms options)
              expanded-forms (condp = cardinality
                               :db.cardinality/one new-expanded-forms
                               :db.cardinality/many (apply deep-merge (vals new-expanded-forms)))]
          (form-option-queries forms queries form expanded-forms param-ctx))))))


(defn form-option-queries "get the form options recursively for all expanded forms"
  [forms queries form expanded-forms param-ctx]
  (apply merge
         (map #(field-queries forms queries expanded-forms param-ctx %)
           form)))
