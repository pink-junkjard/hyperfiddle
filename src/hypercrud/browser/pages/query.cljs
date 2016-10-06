(ns hypercrud.browser.pages.query
  (:require [cljs.pprint :as pprint]
            [clojure.set :as set]
            [hypercrud.client.core :as hc]
            [hypercrud.client.tx :as tx-util]
            [hypercrud.form.field :as field]
            [hypercrud.form.q-util :as q-util]
            [hypercrud.form.util :as form-util]
            [hypercrud.ui.auto-control :refer [auto-control]]
            [hypercrud.ui.form :as form]
            [hypercrud.ui.table :as table]))

(defn initial-entity [q holes-by-name]
  (->> (q-util/parse-holes q)
       (map (juxt identity (fn [hole]
                             (let [code (:hole/formula (get holes-by-name hole))
                                   hole-filler-fn (js/eval code)]
                               (hole-filler-fn)))))
       (into {:db/id -1})))


(defn show-table? [hole-names hp]
  (set/subset? (set hole-names) (set (keys (into {} (remove (comp nil? val) hp))))))


;get query metadata from query edn
(defn get-query [queries q]
  (->> (vals queries)
       (filter #(= q (:query/value %)))
       first))

(defn ui [cur transact! graph forms queries form-id q navigate-cmp]
  (let [query (get-query queries q)
        hole-names (q-util/parse-holes (:query/value query))
        local-statements (cur [:statements] [])
        expanded-cur (cur [:expanded] {})
        holes-by-name (->> (:query/hole query)
                           (map (juxt :hole/name identity))
                           (into {}))
        entity-cur (cur [:holes] (initial-entity q holes-by-name))
        entity @entity-cur
        graph (hc/with graph @local-statements)]
    [:div
     [:h2 (:query/ident query)]
     [:pre (with-out-str (pprint/pprint (:query/value query)))]
     (let [schema (->> holes-by-name
                       (map (fn [[hole-name hole]]
                              [hole-name {:db/cardinality (:attribute/cardinality hole)}]))
                       (into {}))
           stage-tx! (fn [tx]
                       (reset! entity-cur (reduce
                                            #(tx-util/apply-stmt-to-entity schema %1 %2)
                                            entity
                                            tx)))
           form (map #(field/hole->field (get holes-by-name %)) hole-names)]
       [form/form2 graph entity forms queries form expanded-cur stage-tx! navigate-cmp])
     (if (show-table? hole-names entity)                    ;todo what if we have a user hole?
       (let [stage-tx! #(swap! local-statements tx-util/into-tx %)]
         [table/table graph (hc/select graph ::table/query) forms queries form-id expanded-cur stage-tx! navigate-cmp nil]))
     [:button {:key 1 :on-click #(transact! @local-statements)} "Save"]]))


(defn query [state forms queries q form-id param-ctx]
  (let [query (get-query queries q)
        holes-by-name (->> (:query/hole query)
                           (map (juxt :hole/name identity))
                           (into {}))
        hole-names (q-util/parse-holes q)
        form (vec (map field/hole->field (:query/hole query)))
        param-ctx (merge param-ctx (-> (or (get state :holes)
                                           (initial-entity q holes-by-name))
                                       (dissoc :db/id)))]
    (merge
      ;no fields are isComponent true or expanded, so we don't need to pass in a forms map
      (form-util/form-option-queries nil queries form nil param-ctx)

      (if (show-table? hole-names param-ctx)
        (table/query q param-ctx forms queries form-id)))))
