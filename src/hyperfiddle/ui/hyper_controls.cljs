(ns hyperfiddle.ui.hyper-controls
  (:require
    [contrib.reactive :as r]
    [contrib.reagent :refer [fragment]]
    [contrib.ui.input :refer [keyword-input* edn-input*]]
    [contrib.ui.tooltip :refer [tooltip-thick]]
    [hypercrud.browser.context :as context]
    [hypercrud.browser.field :as field]
    [hypercrud.browser.link :as link :refer [rel->link]]
    [hypercrud.ui.control.link-controls :refer [anchors iframes]]
    [hyperfiddle.ui.controls :as controls]
    [hyperfiddle.ui.docstring :refer [semantic-docstring]]
    [hyperfiddle.ui.select :refer [select]]))


(defn label [props ctx]
  (when-let [label (some->> (:hypercrud.browser/field ctx)
                            (r/fmap ::field/label)
                            deref)]
    (let [help-md (semantic-docstring ctx)]
      [tooltip-thick (if help-md
                       [:div.docstring [contrib.ui/markdown help-md]])
       [:label props label (if help-md [:sup "†"])]])))

(defn hyper-select-head [props ctx]
  (let [display-mode (-> @(:hypercrud.ui/display-mode ctx) name keyword)]
    (fragment (if (and (= :xray display-mode)
                       (not (:link/dependent? @(r/track rel->link :options ctx))))
                ; Float right
                [select props ctx])
              [label props ctx]
              [anchors (:hypercrud.browser/path ctx) props ctx link/options-processor]
              [iframes (:hypercrud.browser/path ctx) props ctx link/options-processor])))

(defn hyper-label [props ctx]
  (fragment [label props ctx]
            [anchors (:hypercrud.browser/path ctx) props ctx]
            [iframes (:hypercrud.browser/path ctx) props ctx]))

(defn magic-new-head [props ctx]
  (let [#_#_read-only (r/fmap (comp not controls/writable-entity?) (context/entity ctx)) ;-- don't check this, '* always has a dbid and is writable
        state (r/cursor (:hyperfiddle.ui.form/state ctx) [:hyperfiddle.ui.form/magic-new-a])]
    ;(println (str/format "magic-new-head: %s , %s , %s" @state (pr-str @entity)))
    [keyword-input* @state (r/partial reset! state)
     (merge props {:placeholder ":task/title"})]))

(letfn [(change! [ctx state v]
          (context/with-tx! ctx [[:db/add @(r/fmap :db/id (get-in ctx [:hypercrud.browser/parent :hypercrud.browser/data])) @state v]]))]
  (defn magic-new-body [props ctx]
    (let [read-only (r/fmap (comp not controls/writable-entity?) (get-in ctx [:hypercrud.browser/parent :hypercrud.browser/data]))
          state (r/cursor (:hyperfiddle.ui.form/state ctx) [:hyperfiddle.ui.form/magic-new-a])]
      ;(println (str/format "magic-new-body: %s , %s , %s" @state @read-only (pr-str @entity)))
      ; Uncontrolled widget on purpose i think
      ; Cardinality many is not needed, because as soon as we assoc one value,
      ; we dispatch through a proper typed control
      [edn-input* nil (r/partial change! ctx state)
       (merge props {:read-only (let [_ [@state @read-only]] ; force reactions
                                  (or (nil? @state) @read-only))
                     :placeholder (pr-str "mow the lawn")})])))
