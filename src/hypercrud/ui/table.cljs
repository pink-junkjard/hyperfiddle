(ns hypercrud.ui.table
  (:require [contrib.css :refer [css-slugify classes]]
            [contrib.reactive :as r]
            [hypercrud.browser.system-link :refer [system-link?]]
            [hypercrud.browser.context :as context]
            [hypercrud.browser.link :as link]
            [hypercrud.ui.auto-control :refer [auto-control control-props]]
            [hypercrud.ui.connection-color :as connection-color]
            [hypercrud.ui.control.link-controls :as link-controls]
            [hypercrud.ui.label :refer [label]]))


(defn attr-sortable? [fe attribute ctx]
  (if-let [dbname (some-> (:source-symbol fe) str)]
    (let [{:keys [:db/cardinality :db/valueType]} @(r/cursor (:hypercrud.browser/schemas ctx) [dbname attribute])]
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

(defn relation-keyfn [relation] (hash (map #(or (:db/id %) %) relation)))

(defn sort-fn [sort-col ctx relations-val]
  (let [[sort-fe-pos sort-attr direction] @sort-col
        fe @(r/cursor (:hypercrud.browser/ordered-fes ctx) [sort-fe-pos])
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

; sorting currently breaks click handling in popovers
(defn links-dont-break-sorting? [path ctx]
  (->> @(:hypercrud.browser/links ctx)
       (filter (link/same-path-as? path))
       (remove :link/dependent?)
       (link/options-processor)
       (not-any? link/popover-link?)))

(defn LinkCell [ctx]                                        ; Drive by markdown also (forces unify)
  [(if (:relation ctx) :td.link-cell :th.link-cell)
   (->> (r/unsequence (:hypercrud.browser/ordered-fes ctx))
        (mapcat (fn [[fe i]]
                  (let [ctx (context/find-element ctx i)
                        ctx (context/cell-data ctx)
                        path [(:fe-pos ctx)]]
                    (link-controls/anchors path (not (nil? (:relation ctx))) ctx)
                    ; inline entity-anchors are not yet implemented
                    #_(link-controls/iframes path dependent? ctx)))))])

(defn col-head [ctx]
  (let [field (:hypercrud.browser/field ctx)
        path [(:fe-pos ctx) (:hypercrud.browser/attribute ctx)]
        sortable? (and (attr-sortable? @(:hypercrud.browser/find-element ctx) (:attribute field) ctx)
                       @(r/track links-dont-break-sorting? path ctx))
        sort-direction (let [[sort-fe-pos sort-attr direction] @(::sort-col ctx)]
                         (if (and (= (:fe-pos ctx) sort-fe-pos) (= sort-attr (:attribute field)))
                           direction))
        on-click (fn []
                   (if sortable?
                     (reset! (::sort-col ctx)
                             (case sort-direction
                               :asc [(:fe-pos ctx) (:attribute field) :desc]
                               :desc nil
                               [(:fe-pos ctx) (:attribute field) :asc]))))]
    [:th {:class (classes "hyperfiddle-table-cell"
                          (css-slugify (:hypercrud.browser/attribute ctx))
                          (if sortable? "sortable")
                          (some-> sort-direction name))
          :style {:background-color (connection-color/connection-color ctx)}
          :on-click on-click}
     [label ctx]
     [:div.anchors
      (link-controls/anchors path false ctx link/options-processor)
      (link-controls/iframes path false ctx link/options-processor)]]))

(defn Field "Form fields are label AND value. Table fields are label OR value."
  ([ctx] (Field nil ctx nil))
  ([?f ctx props]
   (if (:relation ctx)
     [:td {:class (classes (:class props) "hyperfiddle-table-cell" "truncate")
           :style {:border-color
                   (let [shadow-link @(r/fmap system-link? (r/cursor (:cell-data ctx) [:db/id]))]
                     (if-not shadow-link (connection-color/connection-color ctx)))}}
      ; todo unsafe execution of user code: control
      [(or ?f (auto-control ctx)) @(:value ctx) ctx (merge (control-props ctx) props)]]
     [col-head ctx])))

(defn table [form ctx]
  (let [sort-col (r/atom nil)]
    (fn [form ctx]
      (let [ctx (assoc ctx :hyperfiddle.ui/layout (:hyperfiddle.ui/layout ctx :hyperfiddle.ui.layout/table)
                           :hypercrud.ui.table/sort-col sort-col
                           ::unp true)]
        [:table.ui-table.unp
         [:thead [form ctx]]
         [:tbody (->> (:relations ctx)
                      (r/unsequence hypercrud.ui.table/relation-keyfn)
                      (map (fn [[relation k]]
                             ^{:key k}
                             [:tr [form (context/relation ctx relation)]]))
                      (doall))]]))))
