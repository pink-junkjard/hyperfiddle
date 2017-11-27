(ns hypercrud.ui.datalist
  (:require [cats.core :as cats :refer-macros [mlet]]
            [hypercrud.browser.browser-ui :as browser-ui]
            [hypercrud.browser.routing :as routing]
            [hypercrud.client.tx :as tx]
            [hypercrud.ui.options :as options]
            [hypercrud.ui.stale :as stale]
            [reagent.core :as reagent]))


;<input list="browsers" name="myBrowser" />
;<datalist id="browsers">
;  <option value="Chrome">
;  <option value="Firefox">
;  <option value="Internet Explorer">
;  <option value="Opera">
;  <option value="Safari">
;  <option value="Microsoft Edge">
;</datalist>

(let [f (fn [ctx data']
          [stale/loading (mlet [{:keys [result ordered-fes ctx]} data'
                                options (options/options result ordered-fes ctx)]
                           (cats/return [:datalist {:id (str (hash (:route ctx)))} options]))
           (fn [e] (browser-ui/ui-error-inline e ctx))
           identity])]
  (defn datalist [options-anchor ctx]
    ; todo this ctx isnt the same when rendering the input vs the datalist
    [browser-ui/clickable-from-anchor options-anchor ctx
     (reagent/partial f ctx)]))

(let [on-change (fn [ctx e]
                  (let [id (options/options-e->id e)]
                    ((:user-with! ctx) (tx/update-entity-attr (:cell-data ctx) (:attribute ctx) id))))]
  (defn datalist-input [value anchor props ctx]
    ; todo this ctx isnt the same when rendering the input vs the datalist
    [stale/loading (routing/build-route' anchor ctx)
     (fn [e] [browser-ui/ui-error-inline e ctx])
     (fn [route]
       [browser-ui/alt-clickable route ctx
        [:input {:list (str (hash route))
                 :value (cond                          ; normalize value for the dom
                          (nil? value) ""
                          :else (str (:db/id value)))
                 :on-change (reagent/partial on-change ctx)
                 :disabled (:read-only props)}]])]))
