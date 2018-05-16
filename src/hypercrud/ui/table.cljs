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
  (if-let [source-symbol (:source-symbol fe)]
    (let [{:keys [:db/cardinality :db/valueType]} @(r/cursor (:hypercrud.browser/schemas ctx) [(str source-symbol) attribute])]
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
  (let [ctx (context/field ctx field)
        path [(:fe-pos ctx) (:hypercrud.browser/attribute ctx)]
        sortable? (and (attr-sortable? @(:hypercrud.browser/find-element ctx) (:attribute field) ctx)
                       @(r/track links-dont-break-sorting? path ctx))
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
     [label ctx]
     [:div.anchors
      (link-controls/anchors path false ctx link/options-processor)
      (link-controls/iframes path false ctx link/options-processor)]]))

(defn LinkCell [dependent? ctx]
  [(if dependent? :td.link-cell :th.link-cell)
   (->> (r/unsequence (:hypercrud.browser/ordered-fes ctx))
        (mapcat (fn [[fe i]]
                  (let [ctx (context/find-element ctx i)
                        ctx (if dependent? (context/cell-data ctx) ctx) ; bit in ctx?
                        path [(:fe-pos ctx)]]
                    (link-controls/anchors path dependent? ctx)
                    ; inline entity-anchors are not yet implemented
                    #_(link-controls/iframes path dependent? ctx)))))])

(defn THead [sort-col ctx]
  [:tr
   (->> (r/unsequence (:hypercrud.browser/ordered-fes ctx))
        (mapcat (fn [[fe i]]
                  (let [ctx (context/find-element ctx i)]   ; "Entity"
                    ; This is contorted to be more parallel to form
                    (->> @(r/cursor (:hypercrud.browser/find-element ctx) [:fields])
                         (map (fn [field]
                                ^{:key (:id field)}
                                [col-head field sort-col ctx])))))))
   [LinkCell false ctx]])

(defn table-cell [control ctx props]
  (let [shadow-link @(r/fmap system-link? (r/cursor (:cell-data ctx) [:db/id]))]
    [:td {:class (classes "hyperfiddle-table-cell" "truncate")
          ; todo use cell renderer for shadow-link styles
          :style {:border-color (if-not shadow-link (connection-color/connection-color ctx))}}
     ; todo unsafe execution of user code: control
     [control ctx (merge (control-props ctx) props)]]))

(defn Cell [ctx]
  [table-cell (auto-control ctx) ctx {}])

(defn Entity [ctx]
  (let [ctx (context/cell-data ctx)]
    (->> (r/cursor (:hypercrud.browser/find-element ctx) [:fields])
         (r/unsequence :id)
         (mapv (fn [[field id]]
                 (let [field @field
                       ctx (-> (context/field ctx field)
                               (context/value (r/fmap (:cell-data->value field) (:cell-data ctx))))]
                   ^{:key id}
                   [Cell ctx]))))))

(defn Relation [ctx]
  (->> (r/unsequence (:hypercrud.browser/ordered-fes ctx))
       (mapcat (fn [[fe i]]
                 (Entity (context/find-element ctx i))))))

(letfn [(sort-fn [sort-col ctx relations-val]
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
        (key-fn [relation] (hash (map #(or (:db/id %) %) relation)))]
  (defn TBody [sort-col ctx]
    (->> (r/fmap (r/partial sort-fn sort-col ctx) (:relations ctx))
         (r/unsequence key-fn)
         (map (fn [[relation key]]
                (let [ctx (context/relation ctx relation)]
                  ^{:key key}
                  [:tr
                   (Relation ctx)                           ; mapcat here
                   [LinkCell true ctx]])))
         (doall))))

(defn Table-inner [ctx]
  (let [sort-col (r/atom nil)]
    (fn [ctx]
      (let [ctx (assoc ctx :layout (:layout ctx :table))]
        [:table.ui-table
         [:thead [THead sort-col ctx]]
         ; Sometimes the leafnode needs all the anchors.
         [:tbody (TBody sort-col ctx)]]))))

(defn Table [ctx]
  ; Relations could be in the ctx, but I chose not for now, because the user renderers don't get to see it i think.
  ; The framework does the mapping and calls userland with a single relation the right number of times.
  [Table-inner ctx])
