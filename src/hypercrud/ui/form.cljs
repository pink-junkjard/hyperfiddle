(ns hypercrud.ui.form
  (:require [hypercrud.ui.auto-control :refer [auto-control]]
            [hypercrud.client.core :as hc]
            [hypercrud.client.tx :as tx-util]
            [hypercrud.compile.eval :as eval]
            [hypercrud.form.util :as form-util]))


(defn field [entity {:keys [graph] {:keys [prompt renderer]} :field :as widget-args}]
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


(defn form [graph entity forms queries form expanded-cur stage-tx! navigate-cmp]
  [:div.form
   (map (fn [{:keys [:ident] :as fieldinfo}]
          ^{:key ident}
          [field entity {:expanded-cur (expanded-cur [ident])
                         :field fieldinfo
                         :forms forms
                         :graph graph
                         :navigate-cmp navigate-cmp
                         :queries queries
                         :stage-tx! stage-tx!}])
        (:form/field form))])


(defn query [dbid forms queries form expanded-forms p-filler param-ctx]
  (let [param-ctx (merge param-ctx {:id (.-id dbid)})
        dbval (get param-ctx :dbval)]
    (if (not (tx-util/tempid? dbid))
      (merge (form-util/form-option-queries forms queries form expanded-forms p-filler param-ctx)
             (let [q '[:find [?e ...] :in $ ?e :where [?e]]
                   pull-exp (form-util/form-pull-exp forms form expanded-forms)]
               {dbid [q [dbval (.-id dbid)] [dbval pull-exp]]}))
      (form-util/form-option-queries forms queries form expanded-forms p-filler param-ctx))))
