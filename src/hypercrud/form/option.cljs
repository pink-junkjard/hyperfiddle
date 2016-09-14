(ns hypercrud.form.option
  (:require [hypercrud.client.core :as hc]))


(defprotocol IFieldOptions
  (label-prop [this])
  (get-key [this])
  (get-option-records [this graph record])

  ;todo should be get-queries and we can delete hc.form.util/field-queries
  (get-query [this])

  (to-string [this entity])
  (parse-string [this s])

  ; todo
  ; cannot create-new if not editable
  ; sometimes might want create-new?=false when editable?=true
  ; we probably want get-create-form and get-edit-form
  ; and then these editable?/create-new? drop out
  (get-form-id [this record])
  (editable? [this record])
  (create-new? [this record]))


(deftype QueryOptions [query args form-id label-prop]
  IFieldOptions
  (label-prop [this] label-prop)

  (get-key [this]
    ;memoizable
    (hash query))

  (get-option-records [this graph record]
    ;memoizable
    (->> (hc/select graph (hash query) query)
         (mapv #(hc/entity graph %))))

  (get-query [this]
    ; todo account for holes
    (let [query-name (hash query)]
      {query-name [query [] '[*]]}))

  (to-string [this entity]
    (str (:db/id entity)))

  (parse-string [this s]
    ; seems like we might want to do more here
    ; e.g. avoid NaN
    (js/parseInt s 10))

  (get-form-id [this record] form-id)
  (editable? [this record] (not= nil form-id))
  (create-new? [this record] (not= nil form-id)))


(deftype SetRecordOptions [fn label-prop]
  IFieldOptions
  (label-prop [this] label-prop)

  (get-key [this]
    ;memoizable
    (hash fn))

  (get-option-records [this graph record]
    ;memoizable
    (->> (fn record)
         (map #(hc/entity graph %))))

  (get-query [this] nil)

  (to-string [this entity]
    ;memoizable
    (str (:db/id entity)))

  (parse-string [this s]
    ; seems like we might want to do more here
    ; e.g. avoid NaN
    (js/parseInt s 10))

  (get-form-id [this record] nil)                           ;todo what should this be
  (editable? [this record] false)                           ;todo what should this be
  (create-new? [this record] false)                         ;todo what should this be
  )


(deftype StaticOptions [static-options label-prop serialize deserialize]
  IFieldOptions
  (label-prop [this] label-prop)
  (get-key [this] (hash static-options))
  (get-option-records [this graph record] static-options)
  (get-query [this] nil)
  (to-string [this entity] (serialize entity))
  (parse-string [this s] (deserialize s))

  (get-form-id [this record] nil)
  (editable? [this record] false)
  (create-new? [this record] false))


(defn gimme-useful-options [{:keys [:label-prop :form :query :inline] :as field}]
  (let [[query args] query]
    (->QueryOptions query args form label-prop)))


(comment
  {:attribute/ident :attribute/isComponent
   :field/prompt "Is Component?"
   :attribute/valueType :boolean
   :db/cardinality :db.cardinality/one

   :field/query1 {:options/query ['[:find [?e ...] :where [?e :form/name]] []]}
   ;dont say my generation
   ;flesh out example in writing with boss pr brand mentions manipulation
   ;maybe explain basic creation of app a bit more, made jump from facebook graph to forking models pretty quickly
   ;use noun 'marketplace' not 'app-store'
   :field/query2 {:options/inline [{:value true :label "true"}
                                   {:value false :label "false"}
                                   {:value nil :label "--"}]}

   :field/query3 {:options/self :studyitem/related-study-items}


   :field/label-prop :label})