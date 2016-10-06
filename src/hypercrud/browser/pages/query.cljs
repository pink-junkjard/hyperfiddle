(ns hypercrud.browser.pages.query
  (:require [cljs.pprint :as pprint]
            [clojure.set :as set]
            [hypercrud.client.core :as hc]
            [hypercrud.client.tx :as tx-util]
            [hypercrud.form.q-util :as q-util]
            [hypercrud.form.util :as form-util]
            [hypercrud.ui.auto-control :refer [auto-control]]
            [hypercrud.ui.form :as form]
            [hypercrud.ui.table :as table]))

(defn initial-entity [{:keys [:query/value] :as query}]
  (->> (q-util/parse-holes value)
       (map (juxt identity (constantly nil)))
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
        entity-cur (cur [:holes] (initial-entity query))
        entity @entity-cur
        graph (hc/with graph @local-statements)]
    [:div
     [:h2 (:query/ident query)]
     [:pre (with-out-str (pprint/pprint (:query/value query)))]
     (let [hole-fields-by-name (->> (:query/hole query)
                                    (map (juxt :ident identity))
                                    (into {}))
           schema (->> hole-fields-by-name
                       (map (fn [[hole-name field]]
                              [hole-name {:db/cardinality (:cardinality field)}]))
                       (into {}))
           stage-tx! (fn [tx]
                       (reset! entity-cur (reduce
                                            #(tx-util/apply-stmt-to-entity schema %1 %2)
                                            entity
                                            tx)))
           form (map #(get hole-fields-by-name %) hole-names)]
       [form/form2 graph entity forms queries form expanded-cur stage-tx! navigate-cmp])
     (if (show-table? hole-names entity)                    ;todo what if we have a user hole?
       (let [stage-tx! #(swap! local-statements tx-util/into-tx %)]
         [table/table graph (hc/select graph ::table/query) forms queries form-id expanded-cur stage-tx! navigate-cmp nil]))
     [:button {:key 1 :on-click #(transact! @local-statements)} "Save"]]))


(defn query [state forms queries q form-id param-ctx]
  (let [query (get-query queries q)
        hole-names (q-util/parse-holes q)
        form (vec (:query/hole query))
        param-ctx (merge param-ctx (-> (or (get state :holes)
                                           (initial-entity query))
                                       (dissoc :db/id)))]
    (merge
      ;no fields are isComponent true or expanded, so we don't need to pass in a forms map
      (form-util/form-option-queries nil queries form nil param-ctx)

      (if (show-table? hole-names param-ctx)
        (table/query q param-ctx forms queries form-id)))))
