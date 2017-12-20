(ns hypercrud.ui.table
  (:require [clojure.string :as string]
            [hypercrud.browser.anchor :as link]
            [hypercrud.browser.auto-anchor :as auto-anchor]
            [hypercrud.browser.context :as context]
            [hypercrud.ui.auto-control :refer [auto-table-cell]]
            [hypercrud.ui.connection-color :as connection-color]
            [hypercrud.ui.form-util :as form-util]
            [hypercrud.ui.user-attribute-renderer :as renderer]
            [hypercrud.ui.control.link-controls :as link-controls]
            [hypercrud.util.reactive :as reactive]))


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
  [:div.anchors
   (link-controls/render-links (remove :link/render-inline? attr-label-anchors) ctx)
   (link-controls/render-inline-links (filter :link/render-inline? attr-label-anchors) ctx)])

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
                                        [fe-pos (:attribute field) :asc]))))
        css-classes [(str "field-attr-" (form-util/css-slugify (str (:attribute field)))) #_"Dustin removed field-id and field-prompt; use a custom renderer"
                     (if sortable? "sortable")
                     (some-> sort-direction name)]]
    [:th {:class (string/join " " css-classes)
          :style {:background-color (connection-color/connection-color (:uri ctx) ctx)}
          :on-click on-click}
     [:label [form-util/field-label field ctx]]
     [col-head-anchors my-links ctx]]))

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

(defn Control [field links props ctx]
  [auto-table-cell field links props ctx])

(defn Field [control field anchors ctx]
  ; why are anchors unused
  (let [shadow-link (auto-anchor/system-anchor? (get-in ctx [:cell-data :db/id]))
        style {:border-color (if-not shadow-link (connection-color/connection-color (:uri ctx) ctx))}]
    [:td.truncate {:style style}
     [control ctx]]))

(defn with-field [Control]
  (fn [field links props ctx]
    [Field (reactive/partial Control field links props) field links ctx]))

(defn Attribute [field anchors props ctx]
  (let [display-mode @(:display-mode ctx)
        Field (case display-mode :xray Field :user (get ctx :field Field))
        Control (case display-mode :xray Control :user (get ctx :control Control))]
    [(with-field Control) field anchors props ctx]))

(defn result-cell [fe cell-data anchors ctx]
  (let [ctx (context/cell-data ctx cell-data)]
    (->> (:fields fe)
         (mapv (fn [field]
                 (let [ctx (-> (context/attribute ctx (:attribute field))
                               (context/value ((:cell-data->value field) (:cell-data ctx)))) ; Not reactive
                       props (form-util/build-props field anchors ctx)]
                   (if (renderer/user-attribute-renderer ctx)
                     (renderer/user-attribute-render field anchors props ctx)
                     ^{:key (:id field)}
                     [Attribute field anchors props ctx])))))))

(defn Relation [relation ordered-fes anchors ctx]
  (->> ordered-fes
       (map-indexed (fn [fe-pos fe]
                      (let [cell-data (get relation fe-pos)
                            ctx (context/find-element ctx fe fe-pos)]
                        (result-cell fe cell-data anchors ctx))))
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

(defn Table [& props]
  (let [sort-col (reactive/atom nil)]
    (fn [relations ordered-fes anchors ctx]
      (let [ctx (assoc ctx :layout (:layout ctx :table))]
        [:table.ui-table
         [:thead [THead ordered-fes anchors sort-col ctx]]
         ; Sometimes the leafnode needs all the anchors.
         [:tbody (TBody relations ordered-fes anchors sort-col ctx)]]))))
