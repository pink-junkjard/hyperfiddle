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

(defn hp->entity [q hp]
  (->> (q-util/parse-holes q)
       (map (juxt identity #(get hp %)))
       (into {:db/id -1})))


(defn show-table? [hole-names hp]
  (set/subset? (set hole-names) (set (keys (into {} (remove (comp nil? val) hp))))))


(defn get-query [queries query-blob]
  (->> (vals queries)
       (filter (fn [query]
                 (= (:q query-blob) (:query/value query))))
       first))

(defn ui [cur transact! graph forms queries form-id query-blob navigate-cmp]
  (let [query (get-query queries query-blob)
        hole-names (q-util/parse-holes (:query/value query))
        local-statements (cur [:statements] [])
        expanded-cur (cur [:expanded] {})
        entity-cur (cur [:holes] (hp->entity (:query/value query) (:hp query-blob)))
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
     [:button {:key 1 :on-click #(transact! @local-statements)} "Save"]

     ;todo just add row at bottom
     [navigate-cmp {:href (str form-id "/entity/-1")} "Create"]]))


(defn query [state forms queries query-blob form-id param-ctx]
  (let [query (get-query queries query-blob)
        q (:q query-blob)
        hole-names (q-util/parse-holes q)
        form (vec (:query/hole query))
        param-ctx (merge param-ctx (-> (or (get state :holes)
                                           (hp->entity q (:hp query-blob)))
                                       (dissoc :db/id)))]
    (merge
      ;no fields are isComponent true or expanded, so we don't need to pass in a forms map
      (form-util/form-option-queries nil queries form nil param-ctx)

      (if (show-table? hole-names param-ctx)
        (table/query q param-ctx forms form-id)))))
