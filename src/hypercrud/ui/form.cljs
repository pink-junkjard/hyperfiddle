(ns hypercrud.ui.form
  (:require [hypercrud.client.tx :as tx-util]
            [hypercrud.compile.eval :as eval]
            [hypercrud.form.util :as form-util]
            [hypercrud.ui.auto-control :refer [auto-control]]))


(defn field [entity {:keys [graph] {:keys [:field/prompt :field/renderer]} :field :as widget-args}]
  [:div.field
   (if prompt [:label prompt])
   (if (empty? renderer)
     [auto-control entity widget-args]
     (let [{renderer :value error :error} (eval/uate (str "(identity " renderer ")"))]
       [:div.value
        (if error
          (pr-str error)
          (try
            (renderer graph entity)
            (catch :default e (pr-str e))))]))])


(defn form [graph entity form expanded-cur stage-tx! navigate-cmp]
  [:div.form
   (map (fn [fieldinfo]
          (let [ident (-> fieldinfo :field/attribute :attribute/ident)]
            ^{:key ident}
            [field entity {:expanded-cur (expanded-cur [ident])
                           :field fieldinfo
                           :graph graph
                           :navigate-cmp navigate-cmp
                           :stage-tx! stage-tx!}]))
        (:form/field form))])


(defn query [dbid form expanded-forms p-filler param-ctx]
  (let [param-ctx (merge param-ctx {:id (.-id dbid)})
        dbval (get param-ctx :dbval)
        option-queries (form-util/form-option-queries form expanded-forms p-filler param-ctx)]
    (if (not (tx-util/tempid? dbid))
      (merge option-queries
             (let [q '[:find [?e ...] :in $ ?e :where [?e]]
                   pull-exp (form-util/form-pull-exp form expanded-forms)]
               {dbid [q [dbval (.-id dbid)] [dbval pull-exp]]}))
      option-queries)))
