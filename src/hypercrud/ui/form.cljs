(ns hypercrud.ui.form
  (:require [contrib.css :refer [css-slugify classes]]
            [contrib.reactive :as r]
            [contrib.reagent :refer [fragment]]
            [contrib.ui.input :as input]
            [hypercrud.browser.link :as link]
            [hypercrud.ui.auto-control :refer [auto-control control-props]]
            [hypercrud.ui.connection-color :as connection-color]
            [hypercrud.ui.control.link-controls :as link-controls]
            [hypercrud.ui.label :refer [auto-label]]))


(defn ui-block-border-wrap [ctx class & children]
  [:div {:class class :style {:border-color (connection-color/connection-color ctx)}}
   (apply fragment :_ children)])

(defn new-field-state-container [ctx]
  (let [attr-ident (r/atom nil)]
    (fn [ctx]
      (let [missing-dbid (nil? @(r/cursor (:cell-data ctx) [:db/id]))]
        (fragment
          :_
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

(defn value "Naked value view, no label, coloring or wrap."
  [?f ctx props]
  (let [props (merge (control-props ctx) props)
        [i a] [(:fe-pos ctx) (:hypercrud.browser/attribute ctx)]]
    (if (:relation ctx)                                     ; dependent or not
      (cond                                                 ; Only attribute is automatic; element renderers can be explicitly asked for though
        a [(or ?f (auto-control ctx)) @(:value ctx) ctx props] ; todo unsafe execution of user code: control
        i (if ?f [?f @(:cell-data ctx) ctx props])
        :naked (if ?f [?f @(:cell-data ctx) ctx props])))))

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
       (if (= a '*)
         ^{:key :new-field} [new-field ctx]
         (fragment
           :_
           (if i
             (fragment :_                                   ;"hyperfiddle-link-entity-independent"
                       (if a
                         ((or (:label-fn props) auto-label) (:hypercrud.browser/field ctx) ctx props))
                       (if (not a)
                         (fragment :_                       ; "hyperfiddle-link-entity-dependent"
                                   (link-controls/anchors path true ctx link/options-processor)
                                   (link-controls/iframes path true ctx link/options-processor)))))
           (if a                                            ; the widget places dependent attr links
             (value ?f ctx props))
           (if (not i)                                      ; if theres not an i, theres not an a
             (link-controls/anchors path false ctx link/options-processor)
             (link-controls/iframes path false ctx link/options-processor))))))))
