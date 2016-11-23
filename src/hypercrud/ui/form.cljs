(ns hypercrud.ui.form
  (:require [hypercrud.client.tx :as tx]
            [hypercrud.compile.eval :as eval]
            [hypercrud.form.util :as form-util]
            [hypercrud.ui.auto-control :refer [auto-control]]))


(defn field [entity {:keys [graph] {:keys [:field/prompt :field/renderer]} :field :as widget-args}]
  [:div.field
   [:label prompt]
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
   (->> (:form/field form)
        (sort-by :field/order)
        (map (fn [fieldinfo]
               (let [ident (-> fieldinfo :field/attribute :attribute/ident)]
                 ^{:key ident}
                 [field entity {:expanded-cur (expanded-cur [ident])
                                :field fieldinfo
                                :graph graph
                                :navigate-cmp navigate-cmp
                                :stage-tx! stage-tx!}]))))])


(defn query [dbid form expanded-forms p-filler param-ctx]
  (let [param-ctx (merge param-ctx {:id (.-id dbid)})
        dbval (get param-ctx :dbval)
        option-queries (form-util/form-option-queries form expanded-forms p-filler param-ctx)]
    (if (not (tx/tempid? dbid))
      (merge option-queries
             {dbid ['[:find ?e :in $ ?e :where [?e]]
                    {"$" dbval "?e" (.-id dbid)}
                    {"?e" [dbval (form-util/form-pull-exp form expanded-forms)]}]})
      option-queries)))
