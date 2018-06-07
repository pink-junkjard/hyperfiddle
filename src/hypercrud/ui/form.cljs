(ns hypercrud.ui.form
  (:require [contrib.css :refer [css-slugify classes]]
            [contrib.reactive :as r]
            [contrib.reagent :refer [fragment]]
            [hypercrud.browser.context :as context]
            [hypercrud.browser.link :as link]
            [hypercrud.ui.auto-control :refer [auto-control control-props]]
            [hypercrud.ui.connection-color :as connection-color]
            [hypercrud.ui.control.link-controls :as link-controls]
            [contrib.ui.input :as input]
            [hypercrud.ui.label :refer [label]]))


(defn ui-block-border-wrap [ctx class & children]
  [:div {:class class :style {:border-color (connection-color/connection-color ctx)}}
   (apply fragment :_ children)])

(defn new-field-state-container [ctx]
  (let [attr-ident (r/atom nil)]
    (fn [ctx]
      (let [missing-dbid (nil? @(r/cursor (:cell-data ctx) [:db/id]))]
        (ui-block-border-wrap
          ctx "field"
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
  ([ctx] (Field nil ctx nil))
  ([?f ctx props]                                           ; fiddle-src wants to fallback by passing nil here explicitly
   (assert @(:hypercrud.ui/display-mode ctx))
   (let [[i a] [(:fe-pos ctx) (:hypercrud.browser/attribute ctx)]
         path (remove nil? [i a])]
     ; Works for entity level stuff too, which will draw entity links and iframes.
     ; Path can be empty
     (ui-block-border-wrap
       ctx (classes "field" "hyperfiddle-form-cell" (:class props) #_":class is for the control, these props came from !cell{}")
       (if i
         (fragment :_                                       ;"hyperfiddle-link-entity-independent"
                   (if a [label ctx])
                   (if (not a)
                     (fragment :_                           ; "hyperfiddle-link-entity-dependent"
                               (link-controls/anchors path true ctx link/options-processor)
                               (link-controls/iframes path true ctx link/options-processor)))))
       (if (and a (not= a '*))
         ; the widget places dependent attr links
         ; todo unsafe execution of user code: control
         [(or ?f (auto-control ctx)) @(:value ctx) ctx (merge (control-props ctx) props)])
       (if (not i) #_(and (not i) (not a))                  ; if theres not an i, theres not an a
         (link-controls/anchors path false ctx link/options-processor)
         (link-controls/iframes path false ctx link/options-processor))))))

