(ns hypercrud.browser.pages.entity
  (:require [hypercrud.form.q-util :as q-util]
            [hypercrud.ui.form :as form]))


(defn ui [cur stage-tx! graph result ordered-forms navigate-cmp]
  [:div
   (map (fn [entity form]
          ^{:key (hash [(.-dbid entity) (.-dbid form)])}
          [form/form graph entity form stage-tx! navigate-cmp])
        result ordered-forms)
   #_[:button {:on-click #(-> (transact! @local-statements)
                              (p/then (fn [{:keys [tempids]}]
                                        (if (tx/tempid? (.-dbid entity))
                                          (navigate! (str "./" (get tempids (.-dbid entity))))))))
               :disabled (empty? @local-statements)}
      (if (tx/tempid? (.-dbid entity)) "Create" "Update")]
   #_[:button {:on-click #(-> (transact! [[:db.fn/retractEntity (.-dbid entity)]])
                              (p/then (fn [_] (navigate! (str "../../../")))))
               :disabled (tx/tempid? (.-dbid entity))}
      "Delete"]])


(defn query [dbid form param-ctx]
  (assert false "todo")                         ; this fn should be fixed and used, currently dead code
  (form/query dbid form q-util/build-params-from-formula param-ctx))
