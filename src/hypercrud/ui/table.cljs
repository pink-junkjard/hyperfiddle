(ns hypercrud.ui.table
  (:require [clojure.string :as string]
            [hypercrud.browser.link :as link]
            [hypercrud.browser.auto-anchor :as auto-anchor]
            [hypercrud.browser.context :as context]
            [hypercrud.browser.result :as result]
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

(defn col-head [field sort-col ctx]
  (let [ctx (context/attribute ctx (:attribute field))
        my-links (->> (link/links-lookup' (:links ctx) [(:fe-pos ctx) (-> ctx :attribute :db/ident)])
                      (remove :link/dependent?))
        [my-links] (link/process-option-links my-links ctx)
        sortable? (and (not-any? link/popover-link? my-links) ; sorting currently breaks click handling in popovers
                       (attr-sortable? (:find-element ctx) (:attribute field) ctx))
        sort-direction (let [[sort-fe-pos sort-attr direction] @sort-col]
                         (if (and (= (:fe-pos ctx) sort-fe-pos) (= sort-attr (:attribute field)))
                           direction))
        on-click (fn []
                   (if sortable?
                     (reset! sort-col (case sort-direction
                                        :asc [(:fe-pos ctx) (:attribute field) :desc]
                                        :desc nil
                                        [(:fe-pos ctx) (:attribute field) :asc]))))]
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

(defn LinkCell [repeating? ctx]
  [(if repeating? :td.link-cell :th.link-cell)
   (->> (:ordered-fes ctx)
        (map-indexed (fn [fe-pos fe]
                       (let [ctx (as-> (context/find-element ctx fe fe-pos) ctx
                                       (if repeating?
                                         (context/cell-data ctx)
                                         ctx))
                             form-anchors (->> (link/links-lookup' (:links ctx) [fe-pos])
                                               ((if repeating? filter remove) :link/dependent?)
                                               ; inline entity-anchors are not yet implemented, what does that mean.
                                               (remove :link/render-inline?))]
                         (link-controls/render-links form-anchors ctx))))
        (apply concat))])

(defn THead [sort-col ctx]
  [:tr
   (->> (:ordered-fes ctx)
        (map-indexed (fn [fe-pos fe]
                       (let [ctx (context/find-element ctx fe fe-pos)]
                         (->> (:fields fe)
                              (map (fn [field]
                                     ^{:key (str (hash fe) "-" (:attribute field))}
                                     [col-head field sort-col ctx]))))))
        (apply concat))
   ; no need for a relation for non-repeating, todo fix this crap
   [LinkCell false ctx]])

(defn table-cell [control -field ctx]
  (let [shadow-link (auto-anchor/system-link? (-> ctx :cell-data deref :db/id))]
    [:td {:class (classes "hyperfiddle-table-cell" "truncate")
          ; todo use cell renderer for shadow-link styles
          :style {:border-color (if-not shadow-link (connection-color/connection-color (:uri ctx) ctx))}}
     [control -field (control-props ctx) ctx]]))

(defn Entity [ctx]
  (let [ctx (context/cell-data ctx)]
    (->> (get-in ctx [:find-element :fields])
         (mapv (fn [field]
                 (let [ctx (-> (context/attribute ctx (:attribute field))
                               (context/value (reactive/map (:cell-data->value field) (:cell-data ctx))))
                       user-cell (case @(:display-mode ctx) :xray table-cell :user table-cell #_(:cell ctx table-cell))]
                   ^{:key (:id field)}
                   [user-cell (auto-control' ctx) field ctx]))))))

(defn Relation [ctx]
  (->> (result/map-relation Entity ctx)
       (apply concat)))

(defn Row [ctx]
  [:tr
   (Relation ctx)
   (LinkCell true ctx)])

(defn TBody [sort-col ctx]
  (let [[sort-fe-pos sort-attr direction] @sort-col
        sort-fn (fn [ctxs]
                  (let [fe (get (:ordered-fes ctx) sort-fe-pos)
                        attr (->> (map :attribute (:fields fe))
                                  (filter #(= % sort-attr))
                                  first)]
                    (if (attr-sortable? fe attr ctx)
                      (let [sort-fn (if sort-attr
                                      ; todo how expensive is N derefs?
                                      #(get-in @(:relation %) [sort-fe-pos sort-attr])
                                      #(get @(:relation %) sort-fe-pos))]
                        (sort-by sort-fn
                                 (case direction
                                   :asc #(compare %1 %2)
                                   :desc #(compare %2 %1))
                                 ctxs))
                      ctxs)))]
    (->> ctx
         (result/map-relations identity)
         sort-fn
         (map (fn [ctx]
                ; todo N derefs
                ^{:key (hash (map #(or (:db/id %) %) @(:relation ctx)))}
                [Row ctx]))
         (doall))))

(defn Table-inner [ctx]
  (let [sort-col (reactive/atom nil)]
    (fn [ctx]
      (let [ctx (assoc ctx :layout (:layout ctx :table))]
        [:table.ui-table
         [:thead [THead sort-col ctx]]
         ; Sometimes the leafnode needs all the anchors.
         [:tbody (TBody sort-col ctx)]]))))

; This one is a appfn
(defn Table [ctx]
  [Table-inner ctx])
