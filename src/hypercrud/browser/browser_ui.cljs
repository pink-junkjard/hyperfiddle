(ns hypercrud.browser.browser-ui
  (:require [cats.core :as cats :refer-macros [mlet]]
            [cats.monad.either :as either :refer-macros [try-either]]
            [hypercrud.browser.anchor :as anchor]
            [hypercrud.browser.base :as base]
            [hypercrud.browser.context :as context]
            [hypercrud.browser.routing :as routing]
            [hypercrud.platform.native-event-listener :refer [native-listener]]
            [hypercrud.platform.safe-render :refer [safe-user-renderer]]
            [hypercrud.state.actions.core :as actions]
            [hypercrud.state.actions.util :as actions-util]
            [hypercrud.ui.stale :as stale]
            [hypercrud.util.core :as util]
            [reagent.core :as reagent]))


(declare ui-from-anchor)

; defn because hypercrud.ui.result/view cannot be required from this ns
(defn f-mode-config []
  {:from-ctx :user-renderer
   :from-link :link/renderer
   :with-user-fn (fn [user-fn]
                   (fn [result ordered-fes anchors ctx]
                     [safe-user-renderer user-fn result ordered-fes anchors ctx]))
   :default hypercrud.ui.result/view})

(letfn [(browse [anchor-index ident ctx & [user-renderer & args]]
          (let [ctx (if user-renderer
                      (assoc ctx :user-renderer user-renderer #_(if f #(apply f %1 %2 %3 %4 args)))
                      ctx)]
            [ui-from-anchor (get anchor-index ident) ctx]))
        (anchor [anchor-index ident ctx label]
          (let [props (-> (anchor/build-anchor-props (get anchor-index ident) ctx)
                          #_(dissoc :style) #_"custom renderers don't want colored links")]
            [(:navigate-cmp ctx) props label]))
        (browse' [anchor-index ident ctx]
          (->> (base/data-from-anchor (get anchor-index ident) ctx)
               (cats/fmap :result)))
        (anchor* [anchor-index ident ctx]
          (anchor/build-anchor-props (get anchor-index ident) ctx))
        (link-fn [anchor-index ident label ctx]
          (js/console.error "Warning: :link-fn is deprecated, and will be removed in a future release. Use :anchor instead")
          (anchor anchor-index ident ctx label))]
  ; process-data returns an Either[Error, DOM]
  (defn process-data [{:keys [result ordered-fes anchors ctx]}]
    (mlet [ui-fn (base/fn-from-mode (f-mode-config) (:fiddle ctx) ctx)
           :let [anchor-index (->> anchors
                                   (filter :anchor/ident)   ; cannot lookup nil idents
                                   (mapv (juxt #(-> % :anchor/ident) identity)) ; [ repeating entity attr ident ]
                                   (into {}))
                 ctx (assoc ctx
                       :anchor (reagent/partial anchor anchor-index)
                       :browse (reagent/partial browse anchor-index)
                       :anchor* (reagent/partial anchor* anchor-index)
                       :browse' (reagent/partial browse' anchor-index)
                       :link-fn (reagent/partial link-fn anchor-index))]]
      (cats/return (ui-fn result ordered-fes anchors ctx)))))

(defn ui-error-inline [e ctx]
  (let [dev-open? (some-> (:dev-open? ctx) deref)
        detail (if dev-open? (str " -- " (pr-str (:data e))))]
    [:code (:message e) " " detail]))

(defn ui-error-block [e ctx]
  #_(ex-message e) #_(pr-str (ex-data e))
  (let [dev-open? (some-> (:dev-open? ctx) deref)
        detail (if dev-open? (util/pprint-str (:data e)))]
    ; todo we don't always return an error with a message
    [:pre (:message e) "\n" detail]))

(defn ui-error [e ctx]
  ; :find-element :attribute :value
  (let [C (cond
            (:ui-error ctx) (:ui-error ctx)                 ; botnav; todo this could throw: hyperfiddle/hyperfiddle#86
            (:attribute ctx) ui-error-inline                ; table: header or cell, form: header or cell
            (:find-element ctx) ui-error-inline             ;
            :else ui-error-block)]                          ; browser including inline true links
    [C e ctx]))

(defn page-on-click [ctx route event]
  (when (and route (.-altKey event))
    ((:dispatch! ctx) (fn [dispatch! get-state]
                        (let [encoded-route (routing/encode route)]
                          (when (actions-util/navigable? encoded-route (get-state))
                            (actions/set-route encoded-route dispatch! get-state)))))
    (.stopPropagation event)))

(defn alt-clickable [route ctx cmp]
  (let [on-click (reagent/partial (or (:page-on-click ctx)
                                      (reagent/partial page-on-click ctx))
                                  route)]
    ^{:key route}
    [native-listener {:on-click on-click} cmp]))

; f :: Either[data] -> Hiccup/Component
(defn clickable-from-route [route ctx f]
  [alt-clickable route ctx
   (f (base/data-from-route route ctx))])

; f :: Either[data] -> Hiccup/Component
(defn clickable-from-anchor [anchor ctx f]
  (let [anchor-props' (try-either (anchor/build-anchor-props anchor ctx)) ; LOOOOOLLLLLL we are dumb
        route (-> (cats/fmap :route anchor-props')
                  (cats/mplus (either/right nil))
                  (cats/extract))]
    [alt-clickable route ctx
     (if (either/left? anchor-props')
       (f anchor-props')
       (let [anchor-props @anchor-props']
         ; todo should filter hidden anchors out before recursing (in widget/render-inline-anchors)
         (if (:hidden anchor-props)
           [:noscript]                                      ; this is WRONGGGGGGG
           (f (mlet [route (routing/build-route' anchor ctx)]
                (base/data-from-route route (context/clean ctx)))))))


     #_(f (mlet [anchor-props anchor-props']
          ; todo should filter hidden anchors out before recursing (in widget/render-inline-anchors)
          (if (:hidden anchor-props)
            (either/right [:noscript])
            (mlet [route (routing/build-route' anchor ctx)]
              (base/data-from-route route (context/clean ctx))))))]))

(defn f [ctx either-v]
  [stale/loading (cats/bind either-v process-data)
   (fn [e] [:div.ui (ui-error e ctx)])
   (fn [v] [:div.ui v])
   (fn [v] [:div.ui.loading v])])

(defn ui-from-route [route ctx]
  (clickable-from-route route ctx (reagent/partial f ctx)))

(defn ui-from-anchor [anchor ctx]
  (clickable-from-anchor anchor ctx (reagent/partial f ctx)))
