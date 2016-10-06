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


(defn field-pull-exp-entry [forms expanded-forms {:keys [ident cardinality options] :as field} expand?]
  (let [new-expanded-forms (get expanded-forms ident)]
    (if (expand? new-expanded-forms field)
      ; components render expanded automatically
      ; so if it is expanded or is a component, pull another level deeper
      (let [form (options->form forms options)]
        {ident (form-pull-exp forms form (condp = cardinality
                                           :db.cardinality/one new-expanded-forms
                                           :db.cardinality/many (apply deep-merge (vals new-expanded-forms)))
                              (fn [expanded-forms {:keys [isComponent]}]
                                (or expanded-forms isComponent)))})

      ; otherwise just add the attribute to the list
      ident)))


(defn form-pull-exp "generate the pull expression recursively for all expanded forms"
  ([forms form expanded-forms]
   (let [expand? (fn [new-expanded-forms {:keys [valueType cardinality isComponent] :as field}]
                   (or new-expanded-forms isComponent (and (= valueType :ref) (= cardinality :db.cardinality/many))))]
     (form-pull-exp forms form expanded-forms expand?)))
  ([forms form expanded-forms expand?]
   (concat
     [:db/id]
     (map #(field-pull-exp-entry forms expanded-forms % expand?) form))))


(defn field-queries [forms queries expanded-forms param-ctx
                     {:keys [ident cardinality valueType isComponent options] :as field}
                     recurse?]
  (merge
    (let [is-ref (= valueType :ref)]
      ; if we are a ref we ALWAYS need the query from the field options
      ; EXCEPT when we are component, in which case no options are rendered, just a form, handled below
      (if (and is-ref (not isComponent)) (option/get-query options queries param-ctx)))

    (let [new-expanded-forms (get expanded-forms ident)]
      ; components render expanded automatically
      ; so if it is expanded or is a component, get the queries for another level deeper
      (if (recurse? new-expanded-forms field)
        (let [form (options->form forms options)
              expanded-forms (condp = cardinality
                               :db.cardinality/one new-expanded-forms
                               :db.cardinality/many (apply deep-merge (vals new-expanded-forms)))]
          (form-option-queries forms queries form expanded-forms param-ctx
                               (fn [expanded-forms {:keys [isComponent] :as field}]
                                 (or expanded-forms isComponent))))))))


(defn form-option-queries "get the form options recursively for all expanded forms"
  ([forms queries form expanded-forms param-ctx]
   (let [recurse? (fn [expanded-forms {:keys [cardinality isComponent] :as field}]
                    (or expanded-forms (= cardinality :db.cardinality/many) isComponent))]
     (form-option-queries forms queries form expanded-forms param-ctx recurse?)))
  ([forms queries form expanded-forms param-ctx recurse?]
   (apply merge
          (map #(field-queries forms queries expanded-forms param-ctx % recurse?)
               form))))
