(ns hypercrud.form.option
  (:require [cljs.reader :as reader]
            [hypercrud.client.core :as hc]
            [hypercrud.types :refer [->Entity]]))


(defprotocol IFieldOptions
  (label-prop [this])
  (get-key [this])
  (get-option-records [this graph entity])

  (has-holes? [this])
  ;todo should be get-queries and we can delete hc.form.util/field-queries
  (get-query [this p-filler label-prop param-ctx])

  ; todo
  ; cannot create-new if not editable
  ; sometimes might want create-new?=false when editable?=true
  ; we probably want get-create-form and get-edit-form
  ; and then these editable?/create-new? drop out
  (get-form [this record])
  (editable? [this record])
  (create-new? [this record]))


(deftype QueryOptions [query form formulas label-prop]
  IFieldOptions
  (label-prop [this] label-prop)

  (get-key [this]
    ;memoizable
    (hash (:query/value query)))

  (get-option-records [this graph entity]
    ;memoizable
    (let [q (reader/read-string (:query/value query))]
      (->> (hc/select graph (hash q) q)
           (mapv #(->Entity % (.-dbval entity))))))

  (has-holes? [this]
    (not (empty? (:query/hole query))))

  (get-query [this p-filler label-prop param-ctx]
    (let [_ (assert (not= nil label-prop) (str "Missing label-prop for " (:query/ident query)))
          q (reader/read-string (:query/value query))
          query-name (hash q)
          params (p-filler query formulas param-ctx)
          pull-dbval (get param-ctx :dbval)
          pull-exp [pull-dbval [:db/id label-prop]]]
      {query-name [q params pull-exp]}))

  (get-form [this record] form)
  (editable? [this record] (not= nil form))
  (create-new? [this record] (not= nil form))

  Object
  (toString [this] (pr-str {:query query
                            :form form
                            :formulas formulas
                            :label-prop label-prop})))


#_(deftype SetRecordOptions [fn label-prop]
    IFieldOptions
    (label-prop [this] label-prop)

    (get-key [this queries]
      ;memoizable
      (hash fn))

    (has-holes? [this queries] false)
    (get-option-records [this queries graph record]
      ;memoizable
      (->> (fn record)
           (map #(hc/entity graph %))))

    (get-query [this queries params] nil)

    (to-string [this entity]
      ;memoizable
      (str (:db/id entity)))

    (parse-string [this s]
      ; seems like we might want to do more here
      ; e.g. avoid NaN
      (js/parseInt s 10))

    (get-form [this record] nil)                            ;todo what should this be
    (editable? [this record] false)                         ;todo what should this be
    (create-new? [this record] false)                       ;todo what should this be
    )


#_(deftype StaticOptions [static-options label-prop serialize deserialize]
    IFieldOptions
    (label-prop [this] label-prop)
    (get-key [this queries] (hash static-options))
    (has-holes? [this queries] false)
    (get-option-records [this queries graph record] static-options)
    (get-query [this queries params] nil)
    (to-string [this entity] (serialize entity))
    (parse-string [this s] (deserialize s))

    (get-form [this record] nil)
    (editable? [this record] false)
    (create-new? [this record] false))


(defn gimme-useful-options [{:keys [:field/label-prop :field/form :field/query :link/formula] :as field}]
  (->QueryOptions query form formula label-prop))


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