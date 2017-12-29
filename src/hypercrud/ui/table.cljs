(ns hypercrud.ui.table
  (:require [clojure.string :as string]
            [hypercrud.browser.link :as link]
            [hypercrud.browser.auto-anchor :as auto-anchor]
            [hypercrud.browser.context :as context]
            [hypercrud.ui.css :refer [css-slugify classes]]
            [hypercrud.ui.auto-control :refer [auto-control' control-props]]
            [hypercrud.ui.connection-color :as connection-color]
            [hypercrud.ui.control.link-controls :as link-controls]
            [hypercrud.util.reactive :as reactive]
            [hypercrud.ui.label :refer [label]]))


(def ^:export with-field identity)                          ; compat
(def ^:export Field nil)                                    ; compat

(defn attr-sortable? [fe attribute ctx]
  (if-let [source-symbol (:source-symbol fe)]
    (let [{:keys [:db/cardinality :db/valueType]} (get-in ctx [:schemas (str source-symbol) attribute])]
      (and
        (= (:db/ident cardinality) :db.cardinality/one)
        ; ref requires more work (inspect label-prop)
        (contains? #{:db.type/keyword
                     :db.type/string
                     :db.type/boolean
                     :db.type/long
                     :db.type/bigint
                     :db.type/float
                     :db.type/double
                     :db.type/bigdec
                     :db.type/instant
                     :db.type/uuid
                     :db.type/uri
                     :db.type/bytes
                     :db.type/code}
                   (:db/ident valueType))))
    (not (nil? fe))))

(defn col-head [fe fe-pos field links sort-col ctx]
  (let [ctx (context/attribute ctx (:attribute field))
        my-links (->> (link/links-lookup' links [fe-pos (-> ctx :attribute :db/ident)])
                      (remove :link/dependent?))
        [my-links] (link/process-option-links my-links ctx)
        sortable? (and (not-any? link/popover-link? my-links) ; sorting currently breaks click handling in popovers
                       (attr-sortable? fe (:attribute field) ctx))
        sort-direction (let [[sort-fe-pos sort-attr direction] @sort-col]
                         (if (and (= fe-pos sort-fe-pos) (= sort-attr (:attribute field)))
                           direction))
        on-click (fn []
                   (if sortable?
                     (reset! sort-col (case sort-direction
                                        :asc [fe-pos (:attribute field) :desc]
                                        :desc nil
                                        [fe-pos (:attribute field) :asc]))))]
    [:th {:class (classes "hyperfiddle-table-cell"
                          (-> ctx :attribute :db/ident str css-slugify)
                          (if sortable? "sortable")
                          (some-> sort-direction name))
          :style {:background-color (connection-color/connection-color (:uri ctx) ctx)}
          :on-click on-click}
     ((:label ctx label) field ctx)
     [:div.anchors
      (link-controls/render-links (remove :link/render-inline? my-links) ctx)
      (link-controls/render-inline-links (filter :link/render-inline? my-links) ctx)]]))

(defn LinkCell [relation repeating? ordered-fes anchors ctx]
  [(if repeating? :td.link-cell :th.link-cell)
   (->> ordered-fes
        (map-indexed (fn [fe-pos fe]
                       (let [ctx (as-> (context/find-element ctx fe fe-pos) ctx
                                       (if repeating?
                                         (context/cell-data ctx (get relation fe-pos))
                                         ctx))
                             form-anchors (->> (link/links-lookup' anchors [fe-pos])
                                               ((if repeating? filter remove) :link/dependent?)
                                               ; inline entity-anchors are not yet implemented, what does that mean.
                                               (remove :link/render-inline?))]
                         (link-controls/render-links form-anchors ctx))))
        (apply concat))])

(defn THead [ordered-fes links sort-col ctx]
  [:tr
   (->> ordered-fes
        (map-indexed (fn [fe-pos fe]
                       (let [ctx (context/find-element ctx fe fe-pos)]
                         (->> (:fields fe)
                              (map (fn [field]
                                     ^{:key (str (hash fe) "-" (:attribute field))}
                                     [col-head fe fe-pos field links sort-col ctx]))))))
        (apply concat))
   ; no need for a relation for non-repeating, todo fix this crap
   [LinkCell nil false ordered-fes links ctx]])

(defn table-cell [control -field links ctx]
  (let [shadow-link (auto-anchor/system-link? (get-in ctx [:cell-data :db/id]))]
    [:td {:class (classes "hyperfiddle-table-cell" "truncate")
          :style {:border-color (if-not shadow-link (connection-color/connection-color (:uri ctx) ctx))}}
     [control -field links (control-props -field links ctx) ctx]]))

(defn Entity [fe cell-data links ctx]
  (let [ctx (context/cell-data ctx cell-data)]
    (->> (:fields fe)
         (mapv (fn [field]
                 (let [ctx (-> (context/attribute ctx (:attribute field))
                               (context/value ((:cell-data->value field) (:cell-data ctx))))
                       user-cell (case @(:display-mode ctx) :xray table-cell :user table-cell #_(:cell ctx table-cell))]
                   ^{:key (:id field)}
                   [user-cell (auto-control' ctx) field links ctx]))))))

(defn Relation [relation ordered-fes anchors ctx]
  (->> ordered-fes
       (map-indexed (fn [fe-pos fe]
                      (let [cell-data (get relation fe-pos)
                            ctx (context/find-element ctx fe fe-pos)]
                        (Entity fe cell-data anchors ctx))))
       (apply concat)))

(defn Row [relation ordered-fes anchors ctx]
  [:tr
   (Relation relation ordered-fes anchors ctx)
   (LinkCell relation true ordered-fes anchors ctx)])

(defn TBody [relations ordered-fes anchors sort-col ctx]
  (let [[sort-fe-pos sort-attr direction] @sort-col
        sort-fn (fn [relations]
                  (let [fe (get ordered-fes sort-fe-pos)
                        attr (->> (map :attribute (:fields fe))
                                  (filter #(= % sort-attr))
                                  first)]
                    (if (attr-sortable? fe attr ctx)
                      (let [sort-fn (if sort-attr
                                      #(get-in % [sort-fe-pos sort-attr])
                                      #(get % sort-fe-pos))]
                        (sort-by sort-fn
                                 (case direction
                                   :asc #(compare %1 %2)
                                   :desc #(compare %2 %1))
                                 relations))
                      relations)))]
    (->> relations
         sort-fn
         (map (fn [relation]
                ^{:key (hash (map #(or (:db/id %) %) relation))}
                [Row relation ordered-fes anchors ctx])))))

(defn Table-inner [props]
  (let [sort-col (reactive/atom nil)]
    (fn [relations ordered-fes anchors ctx]
      (let [ctx (assoc ctx :layout (:layout ctx :table))]
        [:table.ui-table
         [:thead [THead ordered-fes anchors sort-col ctx]]
         ; Sometimes the leafnode needs all the anchors.
         [:tbody (TBody relations ordered-fes anchors sort-col ctx)]]))))

; This one is a function
(defn Table [& props]
  (apply vector Table-inner props))
