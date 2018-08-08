(ns hypercrud.browser.core
  (:require
    [cats.monad.either :as either]
    #?(:cljs [contrib.css :refer [ css]])
    [contrib.reactive :as r]
    #?(:cljs [contrib.reagent :refer [fragment]])
    #?(:cljs [contrib.reagent-native-events :refer [native-click-listener]])
    [hypercrud.browser.base :as base]
    [hypercrud.browser.browser-request :as browser-request]
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
   (defn ui-from-route [route ctx & [?class]]
     (let [click-fn (or (:hypercrud.browser/page-on-click ctx) (constantly nil)) ; parent ctx receives click event, not child frame
           either-v (or (some-> @(runtime/state (:peer ctx) [::runtime/partitions (:branch ctx) :error]) either/left)
                        (base/data-from-route route ctx))
           error-comp (ui-error/error-comp ctx)]
       ; todo the 3 ui fns should be what is injected, not ui-comp
       [stale/loading (stale/can-be-loading? ctx) either-v
        (fn [e]
          (let [on-click (r/partial click-fn route)]
            [native-click-listener {:on-click on-click}
             [error-comp e (css "hyperfiddle-error" ?class "ui")]]))
        (fn [ctx]                                           ; fresh clean ctx
          (let [on-click (r/partial click-fn (:route ctx))]
            [native-click-listener {:on-click on-click}
             (fragment
               [(:alpha.hypercrud.browser/ui-comp ctx) ctx (css ?class "ui")]
               [fiddle-css-renderer (r/cursor (:hypercrud.browser/fiddle ctx) [:fiddle/css])])]))
        (fn [ctx]
          (let [on-click (r/partial click-fn (:route ctx))]
            ; use the stale ctx's route, otherwise alt clicking while loading could take you to the new route, which is jarring
            [native-click-listener {:on-click on-click}
             (fragment
               [(:alpha.hypercrud.browser/ui-comp ctx) ctx (css "hyperfiddle-loading" ?class "ui")]
               [fiddle-css-renderer (r/cursor (:hypercrud.browser/fiddle ctx) [:fiddle/css])])]))])))
