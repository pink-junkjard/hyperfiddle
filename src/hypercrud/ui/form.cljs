(ns hypercrud.ui.form
  (:require [contrib.css :refer [css-slugify classes]]
            [contrib.reactive :as r]
            [contrib.reagent :refer [fragment]]
            [contrib.ui.input :as input]
            [hypercrud.ui.connection-color :as connection-color]
            [hypercrud.ui.label :refer [auto-label]]))


(defn ui-block-border-wrap [ctx class & children]
  [:div {:class class :style {:border-color (connection-color/connection-color ctx)}}
   (apply fragment children)])

(defn new-field-state-container [ctx]
  (let [attr-ident (r/atom nil)]
    (fn [ctx]
      (let [missing-dbid (nil? @(r/cursor (:cell-data ctx) [:db/id]))]
        (fragment
          [:div (let [on-change! #(reset! attr-ident %)]
                  [input/keyword-input* @attr-ident on-change! {:read-only missing-dbid
                                                                :placeholder ":task/title"}])]
          (let [on-change! #(let [id @(r/cursor (:cell-data ctx) [:db/id])]
                              ; todo cardinality many
                              ((:user-with! ctx) [[:db/add id @attr-ident %]]))
                props {:read-only (or missing-dbid (nil? @attr-ident))
                       :placeholder (pr-str "mow the lawn")}]
            [input/edn-input* nil on-change! props]))))))

(defn new-field [ctx]
  ^{:key (hash (keys @(:cell-data ctx)))}
  [new-field-state-container ctx])

(defn Field "Form fields are label AND value. Table fields are label OR value."
  [f ctx props]                                             ; fiddle-src wants to fallback by passing nil here explicitly
  (assert @(:hypercrud.ui/display-mode ctx))
  (ui-block-border-wrap
    ctx (classes "field" "hyperfiddle-form-cell" (:class props) #_":class is for the control, these props came from !cell{}")
    ;(if (= a '*) ^{:key :new-field} [new-field ctx])
    [(or (:label-fn props) auto-label) (:hypercrud.browser/field ctx) (dissoc ctx :relation) props]


    ; Todo, give the right value. It might be the element-level value or nothing.
    [f (some-> ctx :value deref) ctx props]))
