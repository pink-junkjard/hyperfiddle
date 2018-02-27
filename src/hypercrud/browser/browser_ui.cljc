(ns hypercrud.browser.browser-ui
  (:require [cats.core :as cats :refer [mlet]]
            [cats.monad.either :as either]
            [hypercrud.browser.base :as base]
            [hypercrud.browser.context :as context]
            [hypercrud.browser.link :as link]
            [hypercrud.browser.routing :as routing]
    #?(:cljs [hypercrud.react.react-fragment :refer [react-fragment]])
            [hypercrud.ui.css :refer [css-slugify classes]]
            [hypercrud.ui.native-event-listener :refer [native-on-click-listener]]
            [hypercrud.ui.safe-render :refer [safe-user-renderer]]
            [hypercrud.ui.stale :as stale]
            [hypercrud.util.core :as util :refer [unwrap]]
            [hypercrud.util.non-fatal :refer [try-either]]
            [hypercrud.util.reactive :as reactive]
            [hyperfiddle.foundation :as foundation]
            [hyperfiddle.foundation.actions :as foundation-actions]
            [hyperfiddle.runtime :as runtime]))


(declare ui-from-link)

(defn fiddle-css-renderer [s]
  [:style {:dangerouslySetInnerHTML {:__html s}}])

; defn because hypercrud.ui.result/view cannot be required from this ns
(defn f-mode-config []
  {:from-ctx :user-renderer
   :from-link :fiddle/renderer
   :with-user-fn #?(:clj  (assert false "todo")
                    :cljs (fn [user-fn]
                            (fn [result ordered-fes links ctx]
                              #_(react-fragment :_) #_(list)
                              [:div
                               [safe-user-renderer user-fn result ordered-fes links ctx]
                               [fiddle-css-renderer (-> ctx :fiddle :fiddle/css)]])))
   ; todo ui binding should be provided by a RT
   :default #?(:clj  (assert false "todo")
               :cljs (fn [result ordered-fes links ctx]
                       #_(react-fragment :_) #_(list)
                       [:div
                        [hypercrud.ui.result/view result ordered-fes links ctx]
                        [fiddle-css-renderer (-> ctx :fiddle :fiddle/css)]]))})

(letfn [(browse [link-index ident ctx & args]
          (let [kwargs (util/kwargs args)
                [user-renderer & args] (get kwargs nil)
                ctx (if user-renderer
                      (assoc ctx :user-renderer user-renderer #_(if f #(apply f %1 %2 %3 %4 args)))
                      ctx)]
            [ui-from-link (get link-index ident) ctx (:class kwargs)]))
        (anchor [link-index ident ctx label & args]
          (let [kwargs (util/kwargs args)
                props (-> (link/build-link-props (get link-index ident) ctx)
                          #_(dissoc :style) #_"custom renderers don't want colored links")]
            [(:navigate-cmp ctx) props label (:class kwargs)]))
        (browse' [link-index ident ctx]
          (->> (base/data-from-link (get link-index ident) ctx)
               (cats/fmap :result)
               (cats/fmap deref)))
        (anchor* [link-index ident ctx]
          (link/build-link-props (get link-index ident) ctx))]
  ; process-data returns an Either[Error, DOM]
  (defn process-data [{:keys [result ordered-fes links ctx]}]
    (mlet [ui-fn (base/fn-from-mode (f-mode-config) (:fiddle ctx) ctx)
           :let [link-index (->> links
                                 (filter :link/rel)         ; cannot lookup nil idents
                                 (mapv (juxt #(-> % :link/rel) identity)) ; [ repeating entity attr ident ]
                                 (into {}))
                 ctx (assoc ctx
                       :anchor (reactive/partial anchor link-index)
                       :browse (reactive/partial browse link-index)
                       :anchor* (reactive/partial anchor* link-index)
                       :browse' (reactive/partial browse' link-index))]]
      (cats/return (ui-fn @result ordered-fes links ctx)))))

(defn e->map [e]
  (if (map? e)
    e
    {:message #?(:clj  (.getMessage e)
                 :cljs (ex-message e))
     :data (ex-data e)
     :cause #?(:clj  (.getCause e)
               :cljs (ex-cause e))}))

(defn ui-error-inline [e ctx]
  (let [dev-open? true
        {:keys [cause data message]} (e->map e)
        detail (if dev-open? (str " -- " (pr-str data)))]
    [:code message " " detail]))

(defn ui-error-block [e ctx]
  (let [dev-open? true
        {:keys [cause data message]} (e->map e)
        detail (if dev-open? (util/pprint-str data))]
    ; todo we don't always return an error with a message
    [:pre (or message "Error") "\n" detail]))

(defn ui-error [e ctx]
  ; :find-element :attribute :value
  (let [C (cond
            (:hypercrud.ui/ui-error ctx) (:hypercrud.ui/ui-error ctx)
            (:attribute ctx) ui-error-inline                ; table: header or cell, form: header or cell
            (:find-element ctx) ui-error-inline             ;
            :else ui-error-block)]                          ; browser including inline true links
    [C e ctx]))

(defn page-on-click [rt branch branch-aux route event]
  (when (and route (.-altKey event))
    (runtime/dispatch! rt (fn [dispatch! get-state]
                            (when (foundation/navigable? route (get-state))
                              (foundation-actions/set-route rt route branch dispatch! get-state))))
    (.stopPropagation event)))

(defn wrap-ui [either-v route ctx & [class]]
  (let [on-click (reactive/partial (or (:hypercrud.browser/page-on-click ctx) (constantly nil))
                                   route)
        either-v (or (some-> @(runtime/state (:peer ctx) [::runtime/partitions (:branch ctx) :error]) either/left)
                     either-v)]
    [native-on-click-listener {:on-click on-click}
     [stale/loading (stale/can-be-loading? ctx) either-v
      (fn [e] [:div {:class (classes "ui" class "hyperfiddle-error")} (ui-error e ctx)])
      (fn [v] [:div {:class (classes "ui" class)} v])
      (fn [v] [:div {:class (classes "ui" class "hyperfiddle-loading")} v])]]))

(defn ui-from-route [route ctx & [class]]
  [wrap-ui (cats/bind (base/data-from-route route ctx) process-data) route ctx class])

(defn ui-from-link [link ctx & [class]]
  (let [link-props' (try-either (link/build-link-props link ctx))
        v' (mlet [link-props link-props']
             ; todo should filter hidden links out before recursing (in render-inline-links)
             (if (:hidden link-props)
               (either/right [:noscript])
               (mlet [route (routing/build-route' link ctx)
                      ; entire context must be encoded in the route
                      data (base/data-from-route route (context/clean ctx))]
                 (process-data data))))
        route (unwrap (cats/fmap :route link-props'))]
    [wrap-ui v' route ctx (classes class (css-slugify (:link/rel link)))]))
