(ns hypercrud.browser.pages.query
  (:require [cljs.pprint :as pprint]
            [clojure.set :as set]
            [hypercrud.client.core :as hc]
            [hypercrud.client.tx :as tx-util]
            [hypercrud.form.util :as form-util]
            [hypercrud.ui.auto-control :refer [auto-control]]
            [hypercrud.ui.form :as form]
            [hypercrud.ui.table :as table]))

(defn hp->entity [q hp]
  (->> (form-util/parse-holes q)
       (map (juxt identity #(get hp %)))
       (into {:db/id -1})))


(defn show-table? [hole-names hp]
  (set/subset? (set hole-names) (set (keys (into {} (remove (comp nil? val) hp))))))


(defn ui [cur transact! graph forms form-id query query-blob navigate-cmp]
  (let [hole-names (form-util/parse-holes (:query/value query))
        local-statements (cur [:statements] [])
        expanded-cur (cur [:expanded] {})
        entity-cur (cur [:holes] (hp->entity (:query/value query) (:hp query-blob)))
        entity @entity-cur
        graph (hc/with graph @local-statements)]
    [:div
     [:pre (with-out-str (pprint/pprint entity))]
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
       [form/form2 graph entity forms form expanded-cur stage-tx! navigate-cmp])
     (if (show-table? hole-names entity)
       (let [stage-tx! #(swap! local-statements tx-util/into-tx %)]
         [table/table graph (hc/select graph ::table/query) forms form-id expanded-cur stage-tx! navigate-cmp nil]))
     [:button {:key 1 :on-click #(transact! @local-statements)} "Save"]

     ;todo just add row at bottom
     [navigate-cmp {:href (str form-id "/entity/-1")} "Create"]]))


(defn query [state query query-blob forms form-id]
  (let [q (:q query-blob)
        hp (-> (or (get state :holes)
                   (hp->entity q (:hp query-blob)))
               (dissoc :db/id))
        hole-names (form-util/parse-holes q)
        form (vec (:query/hole query))]
    (merge
      ;no fields are isComponent true or expanded, so we don't need to pass in a forms map
      (form-util/form-queries nil form nil)

      (if (show-table? hole-names hp)
        #_(assert (set/subset? (set hole-names) (set (keys hp)))
                  (str "Missing parameters: " (set/difference (set hole-names) (set (keys hp))) " for query: " q))
        (table/query q (map #(get hp %) hole-names) forms form-id (get state :expanded nil)) {}))))
