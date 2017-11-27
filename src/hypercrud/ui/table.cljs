(ns hypercrud.ui.table
  (:require [clojure.string :as string]
            [hypercrud.browser.anchor :as anchor]
            [hypercrud.browser.auto-anchor :as auto-anchor]
            [hypercrud.browser.connection-color :as connection-color]
            [hypercrud.browser.context :as context]
            [hypercrud.ui.auto-control :refer [auto-table-cell]]
            [hypercrud.ui.datalist :refer [datalist]]
            [hypercrud.ui.form-util :as form-util]
            [hypercrud.ui.renderer :as renderer]
            [hypercrud.ui.widget :as widget]
            [reagent.core :as r]))


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

(defn col-head-anchors [attr-label-anchors ctx]
  (let [{anchors false option-anchors true} (group-by anchor/option-anchor? attr-label-anchors)]
    [:div.anchors
     (widget/render-anchors (remove :anchor/render-inline? anchors) ctx)
     (widget/render-inline-anchors (filter :anchor/render-inline? anchors) ctx)
     (->> option-anchors
          (map (fn [anchor]
                 [datalist anchor ctx])))]))

(defn col-head [fe fe-pos field col-anchors sort-col ctx]
  (let [ctx (context/attribute ctx (:attribute field))
        sortable? (and (not-any? anchor/popover-anchor? col-anchors) ; sorting currently breaks click handling in popovers
                       (attr-sortable? fe (:attribute field) ctx))
        sort-direction (let [[sort-fe-pos sort-attr direction] @sort-col]
                         (if (and (= fe-pos sort-fe-pos) (= sort-attr (:attribute field)))
                           direction))
        on-click (fn []
                   (if sortable?
                     (reset! sort-col (case sort-direction
                                        :asc [fe-pos (:attribute field) :desc]
                                        :desc nil
                                        [fe-pos (:attribute field) :asc]))))
        css-classes [(str "field-attr-" (form-util/css-slugify (str (:attribute field)))) #_"Dustin removed field-id and field-prompt; use a custom renderer"
                     (if sortable? "sortable")
                     (some-> sort-direction name)]]
    [:th {:class (string/join " " css-classes)
          :style {:background-color (connection-color/connection-color (:uri ctx) ctx)}
          :on-click on-click}
     [:label [form-util/field-label field ctx]]
     [col-head-anchors col-anchors ctx]]))

(defn LinkCell [relation repeating? ordered-fes anchors-lookup ctx]
  [(if repeating? :td.link-cell :th.link-cell)
   (->> ordered-fes
        (map-indexed (fn [fe-pos fe]
                       (let [ctx (as-> (context/find-element ctx fe) ctx
                                       (if repeating?
                                         (context/cell-data ctx (get relation fe-pos))
                                         ctx))
                             form-anchors (->> (get-in anchors-lookup [fe-pos :links])
                                               ((if repeating? filter remove) :anchor/repeating?)
                                               ; inline entity-anchors are not yet implemented, what does that mean.
                                               (remove :anchor/render-inline?))]
                         (widget/render-anchors form-anchors ctx))))
        (apply concat))])

(defn THead [ordered-fes anchors-lookup sort-col ctx]
  [:tr
   (->> ordered-fes
        (map-indexed (fn [fe-pos fe]
                       (let [ctx (context/find-element ctx fe)]
                         (->> (:fields fe)
                              (map (fn [field]
                                     (let [col-anchors (->> (get-in anchors-lookup [fe-pos (:attribute field) :links])
                                                            (remove :anchor/repeating?))]
                                       ^{:key (str (hash fe) "-" (:attribute field))}
                                       [col-head fe fe-pos field col-anchors sort-col ctx])))))))
        (apply concat))
   ; no need for a relation for non-repeating, todo fix this crap
   [LinkCell nil false ordered-fes anchors-lookup ctx]])

(defn Control [field anchors ctx]
  (let [props (form-util/build-props field anchors ctx)]
    (if (renderer/user-renderer ctx)
      (renderer/user-render field anchors props ctx)
      [auto-table-cell field anchors props ctx])))

(defn Field [control field anchors ctx]
  (let [shadow-link (auto-anchor/system-anchor? (get-in ctx [:cell-data :db/id]))
        style {:border-color (if-not shadow-link (connection-color/connection-color (:uri ctx) ctx))}]
    [:td.truncate {:style style}
     [control ctx]]))

(defn Value [field fe-anchors-lookup ctx]
  (let [ctx (-> (context/attribute ctx (:attribute field))
                (context/value ((:cell-data->value field) (:cell-data ctx)))) ; Not reactive
        display-mode @(:display-mode ctx)
        Field (case display-mode :xray Field :user (get ctx :field Field))
        Control (case display-mode :xray Control :user (get ctx :control Control))
        attr-anchors (get-in fe-anchors-lookup [(:attribute field) :links])]
    [Field (r/partial Control field attr-anchors) field attr-anchors ctx]))

(defn result-cell [fe cell-data fe-anchors-lookup ctx]
  (let [ctx (context/cell-data ctx cell-data)]
    (->> (:fields fe)
         (mapv (fn [field]
                 ^{:key (:id field)}
                 [Value field fe-anchors-lookup ctx])))))

(defn Relation [relation ordered-fes anchors-lookup ctx]
  (->> ordered-fes
       (map-indexed (fn [fe-pos fe]
                      (let [cell-data (get relation fe-pos)
                            fe-anchors-lookup (get anchors-lookup fe-pos)
                            ctx (context/find-element ctx fe)]
                        (result-cell fe cell-data fe-anchors-lookup ctx))))
       (apply concat)))

(defn Row [relation ordered-fes anchors-lookup ctx]
  [:tr
   (Relation relation ordered-fes anchors-lookup ctx)
   (LinkCell relation true ordered-fes anchors-lookup ctx)])

(defn TBody [relations ordered-fes anchors-lookup sort-col ctx]
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
                [Row relation ordered-fes anchors-lookup ctx])))))

(defn Table [& props]
  (let [sort-col (r/atom nil)]
    (fn [relations ordered-fes anchors-lookup ctx]
      (let [ctx (assoc ctx :layout (:layout ctx :table))]
        [:table.ui-table
         [:thead [THead ordered-fes anchors-lookup sort-col ctx]]
         ; Sometimes the leafnode needs all the anchors.
         [:tbody (TBody relations ordered-fes anchors-lookup sort-col ctx)]]))))
