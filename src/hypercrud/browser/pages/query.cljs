(ns hypercrud.browser.pages.query
  (:require [cljs.pprint :as pprint]
            [clojure.set :as set]
            [hypercrud.client.core :as hc]
            [hypercrud.client.tx :as tx-util]
            [hypercrud.form.field :as field]
            [hypercrud.form.util :as form-util]
            [hypercrud.ui.auto-control :refer [auto-control]]
            [hypercrud.ui.table :as table]))

(defn hp->entity [q hp]
  (->> (form-util/parse-holes q)
       (map (juxt identity #(get hp %)))
       (into {:db/id -1})))


(defn show-table? [hole-names hp]
  (set/subset? (set hole-names) (set (keys (into {} (remove (comp nil? val) hp))))))


(defn ui [cur transact! graph forms form-id query query-blob navigate-cmp]
  (let [q (first (:query/value query))
        hole-names (form-util/parse-holes q)
        local-statements (cur [:statements] [])
        expanded-cur (cur [:expanded] {})
        entity-cur (cur [:holes] (hp->entity q (:hp query-blob)))
        entity @entity-cur
        graph (hc/with graph @local-statements)
        stage-tx! #(swap! local-statements tx-util/into-tx %)
        _ (.log js/console (pr-str query))
        _ (.log js/console (pr-str (:query/hole query)))
        holes-by-name (->> (:query/hole query)
                           (map (juxt :hole/name identity))
                           (into {}))]
    [:div
     [:pre (with-out-str (pprint/pprint entity))]
     (doall
       (map (fn [hole-name]
              (let [hole (get holes-by-name hole-name)
                    field (field/field {:prompt (:field/prompt hole)
                                        :ident hole-name
                                        :valueType (:attribute/valueType hole)
                                        :cardinality (:attribute/cardinality hole)
                                        :isComponent false
                                        :options {:label-prop (:field/label-prop hole)
                                                  :form nil
                                                  :query (:field/query hole)}})
                    schema {hole-name {:db/cardinality (:attribute/cardinality hole)}}
                    stage-tx! (fn [tx]
                                (reset! entity-cur (reduce
                                                     #(tx-util/apply-stmt-to-entity schema %1 %2)
                                                     entity
                                                     tx)))]
                [:div.field {:key hole-name}
                 [:label hole-name]
                 [auto-control entity {:expanded-cur (expanded-cur [(:ident field)])
                                       :field field
                                       :forms forms
                                       :graph graph
                                       :navigate-cmp navigate-cmp
                                       :stage-tx! stage-tx!}]]))
            hole-names))
     (if (show-table? hole-names entity)
       [table/table graph (hc/select graph ::table/query) forms form-id expanded-cur stage-tx! navigate-cmp nil])
     [:button {:key 1 :on-click #(transact! @local-statements)} "Save"]

     ;todo just add row at bottom
     [navigate-cmp {:href (str form-id "/entity/-1")} "Create"]]))


(defn query [state query-blob forms form-id]
  (let [q (:q query-blob)
        hp (-> (or (get state :holes)
                   (hp->entity q (:hp query-blob)))
               (dissoc :db/id))
        _ (.log js/console (with-out-str (pprint/pprint hp)))
        hole-names (form-util/parse-holes q)]
    (if (show-table? hole-names hp)
      #_(assert (set/subset? (set hole-names) (set (keys hp)))
                (str "Missing parameters: " (set/difference (set hole-names) (set (keys hp))) " for query: " q))
      (table/query q (map #(get hp %) hole-names) forms form-id (get state :expanded nil)) {})))
