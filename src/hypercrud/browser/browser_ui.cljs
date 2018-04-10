(ns hypercrud.browser.browser-ui
  (:require [cats.core :as cats :refer [mlet]]
            [cats.monad.either :as either]
            [contrib.css :refer [css-slugify classes]]
            [contrib.data :as util :refer [unwrap]]
            [contrib.reactive :as r]
            [contrib.string :refer [memoized-safe-read-edn-string]]
            [contrib.try :refer [try-either]]
            [hypercrud.browser.base :as base]
            [hypercrud.browser.context :as context]
            [hypercrud.browser.link :as link]
            [hypercrud.browser.routing :as routing]
            [hypercrud.ui.error :as ui-error]
    ; [hypercrud.ui.form :as form]
            [hypercrud.ui.native-event-listener :refer [native-on-click-listener]]
            [hypercrud.ui.safe-render :refer [safe-reagent-call]]
            [hypercrud.ui.stale :as stale]
            [hyperfiddle.foundation :as foundation]
            [hyperfiddle.foundation.actions :as foundation-actions]
            [hyperfiddle.runtime :as runtime]))


(declare ui-from-link)

(defn fiddle-css-renderer [s]
  [:style {:dangerouslySetInnerHTML {:__html s}}])

(defn auto-ui-css-class [ctx]
  (classes (let [ident @(r/cursor (:hypercrud.browser/fiddle ctx) [:fiddle/ident])]
             [(css-slugify (some-> ident namespace))
              (css-slugify ident)])))

; defn because hypercrud.ui.result/view cannot be required from this ns
(defn f-mode-config []
  {:from-ctx :user-renderer
   :from-fiddle (fn [fiddle] @(r/cursor fiddle [:fiddle/renderer]))
   :with-user-fn (fn [user-fn]
                   (fn [ctx]
                     #_(fragment :_) #_(list)
                     [:div
                      [safe-reagent-call (ui-error/error-comp ctx) user-fn ctx (auto-ui-css-class ctx)]
                      [fiddle-css-renderer @(r/cursor (:hypercrud.browser/fiddle ctx) [:fiddle/css])]]))
   ; todo ui binding should be provided by a RT
   :default (fn [ctx]
              #_(fragment :_) #_(list)
              [:div
               [hypercrud.ui.result/fiddle-xray ctx (auto-ui-css-class ctx)]
               [fiddle-css-renderer @(r/cursor (:hypercrud.browser/fiddle ctx) [:fiddle/css])]])})

(letfn [(browse [rel #_dependent? path ctx & args]
          (let [{[user-renderer & args] nil :as kwargs} (util/kwargs args)
                {:keys [:link/dependent? :link/path] :as link} @(r/track link/rel->link rel path ctx)
                ctx (-> (context/relation-path ctx (into [dependent?] (unwrap (memoized-safe-read-edn-string (str "[" path "]")))))
                        (as-> ctx (if user-renderer (assoc ctx :user-renderer user-renderer #_(if f #(apply f %1 %2 %3 %4 args))) ctx)))]
            [ui-from-link link ctx (:class kwargs)]))
        (anchor [rel #_dependent? path ctx label & args]
          (let [kwargs (util/kwargs args)
                {:keys [:link/dependent? :link/path] :as link} @(r/track link/rel->link rel path ctx)
                ctx (context/relation-path ctx (into [dependent?] (unwrap (memoized-safe-read-edn-string (str "[" path "]")))))
                props (-> (link/build-link-props link ctx)
                          #_(dissoc :style) #_"custom renderers don't want colored links")]
            [(:navigate-cmp ctx) props label (:class kwargs)]))
        (cell [[d i a] ctx & args]                          ; form only
          [hypercrud.ui.form/Cell (context/relation-path ctx [d i a])])
        (value [path ctx & args]
          (let [{[f & args] nil :as kwargs} (util/kwargs args)
                ctx (context/relation-path ctx path)
                field (:hypercrud.browser/field ctx)
                #_#_control-props (merge (hypercrud.ui.auto-control/control-props ctx) kwargs)]
            [(or f (partial hypercrud.ui.auto-control/auto-control field {} nil)) ctx]))
        (browse' [rel #_dependent? path ctx]
          (->> (base/data-from-link @(r/track link/rel->link rel path ctx) ctx)
               (cats/fmap :hypercrud.browser/result)
               (cats/fmap deref)))
        (anchor* [rel #_dependent? path ctx]
          (link/build-link-props @(r/track link/rel->link rel path ctx) ctx))]
  ; convenience functions, should be declared fns in this or another ns and accessed out of band of ctx
  (defn ui-bindings [ctx]
    (assoc ctx
      :anchor anchor
      :browse browse
      :cell cell
      :value value
      :anchor* anchor*
      :browse' browse')))

(defn page-on-click [rt branch branch-aux route event]
  (when (and route (.-altKey event))
    (runtime/dispatch! rt (fn [dispatch! get-state]
                            (when (foundation/navigable? route (get-state))
                              (foundation-actions/set-route rt route branch false dispatch! get-state))))
    (.stopPropagation event)))

(defn wrap-ui [either-v route ctx & [class]]
  (let [on-click (r/partial (or (:hypercrud.browser/page-on-click ctx) (constantly nil))
                            route)
        either-v (or (some-> @(runtime/state (:peer ctx) [::runtime/partitions (:branch ctx) :error]) either/left)
                     either-v)
        error-comp (ui-error/error-comp ctx)]
    [native-on-click-listener {:on-click on-click}
     [stale/loading (stale/can-be-loading? ctx) either-v
      (fn [e] [:div {:class (classes "ui" class "hyperfiddle-error")} [error-comp e]])
      (fn [v] [:div {:class (classes "ui" class)} v])
      (fn [v] [:div {:class (classes "ui" class "hyperfiddle-loading")} v])]]))

(defn hf-ui [route ctx]                                     ; returns an Either[Error, DOM]
  (mlet [ctx (base/data-from-route route ctx)
         ui-fn (base/fn-from-mode (f-mode-config) (:hypercrud.browser/fiddle ctx) ctx)]
    (cats/return (ui-fn (ui-bindings ctx)))))

(defn ui-from-route [route ctx & [class]]
  [wrap-ui (hf-ui route ctx) route ctx class])

(defn ui-from-link [link ctx & [class]]
  (let [link-props' (try-either (link/build-link-props link ctx))
        v' (mlet [link-props link-props']
             ; todo should filter hidden links out before recursing (in render-inline-links)
             (if (:hidden link-props)
               (either/right [:noscript])
               (cats/bind (routing/build-route' link ctx)
                          #(hf-ui % (context/clean ctx)))))
        route (unwrap (cats/fmap :route link-props'))]
    [wrap-ui v' route ctx (classes class (css-slugify (:link/rel link)))]))
