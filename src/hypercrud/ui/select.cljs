(ns hypercrud.ui.select
  (:require [cats.monad.exception :as exception]
            [hypercrud.client.tx :as tx]
            [hypercrud.form.option :as option]
            [hypercrud.types :refer [->DbId]]))


(defn select-boolean* [value field {:keys [user-swap!] :as param-ctx}]
  (let [{:keys [:attribute/ident] :as attribute} (:field/attribute field)
        props {;; normalize value for the dom - value is either nil, an :ident (keyword), or eid
               :value (if (nil? value) "" (str value))
               ;; reconstruct the typed value
               :on-change #(let [v (case (.-target.value %)
                                     "" nil
                                     "true" true
                                     "false" false)]
                             (user-swap! {:tx (tx/update-entity-attr (:entity param-ctx) attribute v)}))}]
    [:select props
     [:option {:key true :value "true"} "True"]
     [:option {:key false :value "false"} "False"]
     [:option {:key :nil :value ""} "--"]]))


(defn select* [value maybe-field param-ctx]
  ; value :: {:db/id #DbId[17592186045891 17592186045422]}
  (let [props {;; normalize value for the dom - value is either nil, an :ident (keyword), or eid
               :value (cond
                        (nil? value) ""
                        :else (-> value :db/id :id str))

               ;; reconstruct the typed value
               :on-change #(let [select-value (.-target.value %)
                                 dbid (cond
                                        (= "" select-value) nil
                                        :else-hc-select-option-node (->DbId (js/parseInt select-value 10) (-> value :db/id :conn-id)))]
                             ((:user-swap! param-ctx) {:tx (tx/update-entity-attr (:entity param-ctx) (:attribute param-ctx) dbid)}))}]

    ; Having options is not required; maybe the form isn't modeled yet.
    (let [option-records (if maybe-field (option/get-option-records maybe-field param-ctx) (exception/success []))]
      (if (exception/failure? option-records)
        [:span {:on-click #(js/alert (pr-str (.-e option-records)))} "Failed to hydrate"]
        (let [option-records (.-v option-records)]
          #_(assert (or (nil? value)
                        (tx/tempid? (:db/id value))
                        (nil? option-records)               ; user hasn't picked the query yet but may be about to
                        (contains? (set option-records) value)) (str "Select options does not contain selected value: " (pr-str value)))
          [:select.select props (-> (->> option-records
                                         (mapv (fn [result]
                                                 (assert (= 1 (count result)) "Cannot use multiple find-elements for an options-link") ; why not?
                                                 (let [entity (first result)
                                                       dbid (:db/id entity)
                                                       label-prop (option/label-prop maybe-field result)]
                                                   [dbid label-prop])))
                                         (sort-by second)
                                         (mapv (fn [[dbid label-prop]]
                                                 ^{:key dbid}
                                                 [:option {:value (.-id dbid)} label-prop])))
                                    (concat
                                      [[:option {:key :blank :value ""} "--"]]))])))))
