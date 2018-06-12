(ns hypercrud.browser.core
  (:require
    [cats.core :as cats]
    [cats.monad.either :as either]
    #?(:cljs [contrib.css :refer [css-slugify css]])
    [contrib.data :refer [kwargs]]
    [contrib.reactive :as r]
    [contrib.try :refer [try-either]]
    #?(:cljs [contrib.ui.native-event-listener :refer [native-on-click-listener]])
    [hypercrud.browser.base :as base]
    [hypercrud.browser.browser-request :as browser-request]
    [hypercrud.browser.link :as link]
    [hypercrud.browser.routing :as routing]
    #?(:cljs [hypercrud.ui.error :as ui-error])
    #?(:cljs [hypercrud.ui.stale :as stale])
    [hyperfiddle.runtime :as runtime]))


(def data base/data-from-link)
(def request browser-request/request-from-link)

; this interface is to reuse an evaluated route (performance of eval and effects)
; and also the entrypoint (route is document.location, original anchor is unknowable)
(def data-from-route base/data-from-route)
(def request-from-route browser-request/request-from-route)

#?(:cljs (defn- fiddle-css-renderer [s] [:style {:dangerouslySetInnerHTML {:__html @s}}]))

#?(:cljs
   (defn ui-from-route [route ctx & [class]]
     (let [click-fn (or (:hypercrud.browser/page-on-click ctx) (constantly nil)) ; parent ctx receives click event, not child frame
           either-v (or (some-> @(runtime/state (:peer ctx) [::runtime/partitions (:branch ctx) :error]) either/left)
                        (base/data-from-route route ctx))
           error-comp (ui-error/error-comp ctx)]
       ; todo the 3 ui fns should be what is injected, not ui-comp
       [stale/loading (stale/can-be-loading? ctx) either-v
        (fn [e]
          (let [on-click (r/partial click-fn route)]
            [native-on-click-listener {:on-click on-click}
             [:div {:class (css "ui" class "hyperfiddle-error")}
              [error-comp e]]]))
        (fn [ctx]
          (let [on-click (r/partial click-fn (:route ctx))]
            [native-on-click-listener {:on-click on-click}
             [:div {:class (css "ui" class)}                ; fragment
              [(:alpha.hypercrud.browser/ui-comp ctx) ctx]
              [fiddle-css-renderer (r/cursor (:hypercrud.browser/fiddle ctx) [:fiddle/css])]]]))
        (fn [ctx]
          (let [on-click (r/partial click-fn (:route ctx))]
            ; use the stale ctx's route, otherwise alt clicking while loading could take you to the new route, which is jarring
            [native-on-click-listener {:on-click on-click}
             [:div {:class (css "ui" class "hyperfiddle-loading")}
              [(:alpha.hypercrud.browser/ui-comp ctx) ctx]
              [fiddle-css-renderer (r/cursor (:hypercrud.browser/fiddle ctx) [:fiddle/css])]]]))])))

#?(:cljs
   (defn ui [link ctx ?class & args]                        ; TODO don't omit user props
     (let [props (kwargs args)
           error-comp (ui-error/error-comp ctx)
           hidden' (->> (try-either (link/build-link-props link ctx)) ; todo we want the actual error from the link props
                        (cats/fmap :hidden))]
       [stale/loading (stale/can-be-loading? ctx) hidden'
        (fn [e] [error-comp e])
        (fn [link-props]
          (if (:hidden link-props)
            [:noscript]
            [stale/loading (stale/can-be-loading? ctx) (routing/build-route' link ctx (:frag props))
             (fn [e] [error-comp e])
             (fn [route] [ui-from-route route ctx (css ?class (css-slugify (:link/rel link)))])]))])))
