(ns hypercrud.ui.table
  (:require [hypercrud.browser.auto-anchor :as auto-anchor]
            [hypercrud.browser.context :as context]
            [hypercrud.browser.find-element :as find-element]
            [hypercrud.browser.link :as link]
            [hypercrud.ui.auto-control :refer [auto-control' control-props]]
            [hypercrud.ui.connection-color :as connection-color]
            [hypercrud.ui.control.link-controls :as link-controls]
            [hypercrud.ui.css :refer [css-slugify classes]]
            [hypercrud.ui.label :refer [label]]
            [hypercrud.util.reactive :as reactive]))


(defn attr-sortable? [fe attribute ctx]
  (if-let [source-symbol (:source-symbol fe)]
    (let [{:keys [:db/cardinality :db/valueType]} @(reactive/cursor (:hypercrud.browser/schemas ctx) [(str source-symbol) attribute])]
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

; sorting currently breaks click handling in popovers
(defn links-dont-break-sorting? [path ctx]
  (->> @(:hypercrud.browser/links ctx)
       (filter (link/same-path-as? path))
       (remove :link/dependent?)
       (link/options-processor)
       (not-any? link/popover-link?)))

(defn col-head [field sort-col ctx]
  (let [ctx (context/attribute ctx (:attribute field))
        path [(:fe-pos ctx) (:hypercrud.browser/attribute ctx)]
        sortable? (and (attr-sortable? @(:hypercrud.browser/find-element ctx) (:attribute field) ctx)
                       @(reactive/track links-dont-break-sorting? path ctx))
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
                          (css-slugify (:hypercrud.browser/attribute ctx))
                          (if sortable? "sortable")
                          (some-> sort-direction name))
          :style {:background-color (connection-color/connection-color ctx)}
          :on-click on-click}
     ; todo unsafe execution of user code: label
     ((:label ctx (partial vector label)) field ctx)
     [:div.anchors
      (link-controls/render-nav-cmps path false ctx link/options-processor)
      (link-controls/render-inline-links path false ctx link/options-processor)]]))

(defn LinkCell [dependent? relation ctx]
  [(if dependent? :td.link-cell :th.link-cell)
   (->> (find-element/fe-ctxs ctx)
        (mapcat (fn [ctx]
                  (let [ctx (if dependent?
                              (context/cell-data ctx relation)
                              ctx)
                        path [(:fe-pos ctx)]]
                    (link-controls/render-nav-cmps path dependent? ctx)
                    ; inline entity-anchors are not yet implemented
                    #_(link-controls/render-inline-links path dependent? ctx)))))])

(defn THead [sort-col ctx]
  [:tr
   (->> (find-element/fe-ctxs ctx)
        (mapcat (fn [ctx]
                  (->> @(reactive/cursor (:hypercrud.browser/find-element ctx) [:fields])
                       (map (fn [field]
                              ^{:key (:id field)}
                              [col-head field sort-col ctx]))))))
   ; no need for a relation for non-repeating, todo fix this crap
   [LinkCell false nil ctx]])

(defn table-cell [control -field ctx]
  (let [shadow-link @(reactive/fmap auto-anchor/system-link? (reactive/cursor (:cell-data ctx) [:db/id]))]
    [:td {:class (classes "hyperfiddle-table-cell" "truncate")
          ; todo use cell renderer for shadow-link styles
          :style {:border-color (if-not shadow-link (connection-color/connection-color ctx))}}
     ; todo unsafe execution of user code: control
     [control -field (control-props ctx) ctx]]))

(defn Cell [field ctx]
  (let [ctx (-> (context/attribute ctx (:attribute field))
                (context/value (reactive/fmap (:cell-data->value field) (:cell-data ctx))))
        user-cell (case @(:hypercrud.ui/display-mode ctx) :xray table-cell :user table-cell #_(:cell ctx table-cell))]
    [user-cell (auto-control' ctx) field ctx]))

(defn Entity [relation ctx]
  (let [ctx (context/cell-data ctx relation)]
    (->> @(reactive/cursor (:hypercrud.browser/find-element ctx) [:fields])
         (mapv (fn [field]
                 ^{:key (:id field)}
                 [Cell field ctx])))))

(defn Relation [relation ctx]
  (->> (find-element/fe-ctxs ctx)
       (mapcat (partial Entity relation))))

(defn Row [relation ctx]
  [:tr
   (Relation relation ctx)
   [LinkCell true relation ctx]])

(letfn [(sort-fn [sort-col ctx relations-val]
          (let [[sort-fe-pos sort-attr direction] @sort-col
                fe @(reactive/cursor (:hypercrud.browser/ordered-fes ctx) [sort-fe-pos])
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
                         relations-val))
              relations-val)))
        (key-fn [relation] (hash (map #(or (:db/id %) %) relation)))]
  (defn TBody [sort-col relations ctx]
    (->> (reactive/fmap (reactive/partial sort-fn sort-col ctx) relations)
         (reactive/unsequence key-fn)
         (map (fn [[relation key]]
                ^{:key key}
                [Row relation ctx]))
         (doall))))

(defn Table-inner [relations ctx]
  (let [sort-col (reactive/atom nil)]
    (fn [relations ctx]
      (let [ctx (assoc ctx :layout (:layout ctx :table))]
        [:table.ui-table
         [:thead [THead sort-col ctx]]
         ; Sometimes the leafnode needs all the anchors.
         [:tbody (TBody sort-col relations ctx)]]))))

; This one is a appfn
(defn Table [relations ctx]
  [Table-inner relations ctx])
