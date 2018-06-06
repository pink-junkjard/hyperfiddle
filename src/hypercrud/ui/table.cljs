(ns hypercrud.ui.table
  (:require [contrib.css :refer [css-slugify classes]]
            [contrib.reactive :as r]
            [hypercrud.browser.system-link :refer [system-link?]]
            [hypercrud.browser.context :as context]
            [hypercrud.browser.link :as link]
            [hypercrud.ui.auto-control :refer [auto-control control-props]]
            [hypercrud.ui.connection-color :as connection-color]
            [hypercrud.ui.control.link-controls :as link-controls]
            [hypercrud.ui.label :refer [label]]
            [hyperfiddle.data :as hf]))


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
        sortable? (and (hf/attr-sortable? @(:hypercrud.browser/find-element ctx) (:attribute field) ctx)
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
