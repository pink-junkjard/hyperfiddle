(ns hypercrud.form.option
  (:require [cljs.reader :as reader]
            [hypercrud.client.core :as hc]
            [hypercrud.types :refer [->DbVal]]
            [hypercrud.util :as util]))


(defprotocol IFieldOptions
  (label-prop [this])
  (get-key [this])
  (get-option-records [this graph entity])

  (has-holes? [this])
  ;todo should be get-queries and we can delete hc.form.util/field-queries
  (get-query [this p-filler param-ctx])

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

  (get-key [this]                                           ; for remounting the whole select
    ;memoizable
    (hash [(:query/value query) label-prop]))

  (get-option-records [this graph entity]
    ;memoizable
    (if-let [q (:query/value query)]
      (let [q (reader/read-string q)
            dbgraph (.-dbgraph entity)                      ;todo this is wrong
            ]
        (->> (hc/select graph (hash q) q)
             (mapv #(hc/entity dbgraph (first %)))))))      ;todo first is a hack

  (has-holes? [this]
    (not (empty? (:query/hole query))))

  (get-query [this p-filler param-ctx]
    (if-let [q (:query/value query)]
      (let [q (reader/read-string q)
            query-name (hash q)
            params (p-filler query formulas param-ctx)
            pull-dbval (get param-ctx :dbval)
            find-element (str (first (util/parse-query-element q :find))) ;todo this is a hack
            pull-exp {find-element [pull-dbval (if label-prop [:db/id label-prop] [:db/id])]}]
        {query-name [q params pull-exp]})))

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
   :field/query2 {:options/inline [{:value true :label "true"}
                                   {:value false :label "false"}
                                   {:value nil :label "--"}]}

   :field/query3 {:options/self :studyitem/related-study-items}


   :field/label-prop :label})