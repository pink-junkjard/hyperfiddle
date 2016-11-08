(ns hypercrud.form.util
  (:require [hypercrud.form.option :as option]))


(defn- deep-merge "recursively merges maps. if vals are not maps, will throw (which is a very particular
case that works for expanded-forms)"
  [& vals]
  (apply merge-with deep-merge vals))


(declare form-pull-exp)
(declare form-option-queries)


(defn field-pull-exp-entry [expanded-forms field expand?]
  (let [{:keys [:attribute/ident :attribute/cardinality]} (:field/attribute field)
        new-expanded-forms (get expanded-forms ident)]
    (if (expand? new-expanded-forms (:field/attribute field))
      ; components render expanded automatically
      ; so if it is expanded or is a component, pull another level deeper
      (let [form (-> (option/gimme-useful-options field)
                     (option/get-form "todo"))]
        {ident (form-pull-exp form (condp = (:db/ident cardinality)
                                     :db.cardinality/one new-expanded-forms
                                     :db.cardinality/many (apply deep-merge (vals new-expanded-forms)))
                              (fn [expanded-forms {:keys [:attribute/isComponent]}]
                                (or expanded-forms isComponent)))})

      ; otherwise just add the attribute to the list
      ident)))


(defn expand? [new-expanded-forms
               {:keys [:attribute/valueType :attribute/cardinality :attribute/isComponent]}]
  (or new-expanded-forms
      isComponent
      (and (= (:db/ident valueType) :db.type/ref)
           (= (:db/ident cardinality) :db.cardinality/many))))


(defn form-pull-exp "generate the pull expression recursively for all expanded forms"
  ([form expanded-forms]
   (form-pull-exp form expanded-forms expand?))
  ([form expanded-forms expand?]
   (concat
     [:db/id]
     (map #(field-pull-exp-entry expanded-forms % expand?) (:form/field form)))))


(defn dont-go-deeper-except-components? [expanded-forms field]
  (or expanded-forms (-> field :field/attribute :attribute/isComponent)))

(defn recurse? [expanded-forms field]
  (or expanded-forms
      (let [{:keys [:attribute/cardinality :attribute/isComponent]} (:field/attribute field)]
        (or (= (:db/ident cardinality) :db.cardinality/many)
            isComponent))))


(defn field-queries [expanded-forms p-filler param-ctx field recurse?]
  (let [{:keys [:attribute/ident :attribute/cardinality :attribute/valueType :attribute/isComponent]} (:field/attribute field)
        options (option/gimme-useful-options field)]
    (merge
      (let [is-ref (= (:db/ident valueType) :db.type/ref)]
        ; if we are a ref we ALWAYS need the query from the field options
        ; EXCEPT when we are component, in which case no options are rendered, just a form, handled below
        (if (and is-ref (not isComponent))
          (option/get-query options p-filler (option/label-prop options) param-ctx)))

      (let [new-expanded-forms (get expanded-forms ident)]
        ; components render expanded automatically
        ; so if it is expanded or is a component, get the queries for another level deeper
        (if (recurse? new-expanded-forms field)
          (let [form (-> (option/gimme-useful-options field)
                         (option/get-form "todo"))
                expanded-forms (condp = (:db/ident cardinality)
                                 :db.cardinality/one new-expanded-forms
                                 :db.cardinality/many (apply deep-merge (vals new-expanded-forms)))]
            (form-option-queries form expanded-forms p-filler param-ctx
                                 (condp = (:db/ident cardinality)
                                   ;; if this form is expanded, recurse like normal, we dont know how deep to go yet
                                   :db.cardinality/one recurse?
                                   ;; this is a table renderer, only so deep you can go - prevent infinite expansions/nested forms
                                   :db.cardinality/many dont-go-deeper-except-components?))))))))


(defn form-option-queries "get the form options recursively for all expanded forms"
  ([form expanded-forms p-filler param-ctx]
   (form-option-queries form expanded-forms p-filler param-ctx recurse?))
  ([form expanded-forms p-filler param-ctx recurse?]
   (apply merge
          (map #(field-queries expanded-forms p-filler param-ctx % recurse?)
               (:form/field form)))))
