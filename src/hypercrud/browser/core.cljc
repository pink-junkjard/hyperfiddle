(ns hypercrud.browser.core
  (:require
    [cats.core :as cats]
    [cats.monad.either :as either]
    #?(:cljs [contrib.css :refer [css-slugify css]])
    [contrib.data :refer [kwargs]]
    [contrib.reactive :as r]
    #?(:cljs [contrib.reagent :refer [fragment]])
    [contrib.try :refer [try-either]]
    #?(:cljs [contrib.reagent-native-events :refer [native-click-listener]])
    [hypercrud.browser.base :as base]
    [hypercrud.browser.browser-request :as browser-request]
    [hypercrud.browser.link :as link]
    #?(:cljs [hypercrud.ui.error :as ui-error])
    #?(:cljs [hypercrud.ui.stale :as stale])
    [hyperfiddle.runtime :as runtime]
    [hypercrud.browser.routing :as routing]
    [hypercrud.browser.router :as router]
    [hypercrud.browser.context :as context]))


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

#?(:cljs
   (defn ui [link ctx ?class & [props]]                     ; TODO don't omit user props
     (let [error-comp (ui-error/error-comp ctx)
           link-props' (try-either (link/build-link-props link ctx))]
       [stale/loading (stale/can-be-loading? ctx) (cats/fmap :hidden link-props') ; todo we want the actual error from the link props
        (fn [e] [error-comp e])
        (fn [link-props]
          (if (:hidden link-props)
            [:noscript]
            ; link-props swallows bad routes (shorts them to nil),
            ; all errors will always route through as (either/right nil)
            (let [route' (routing/build-route' link (context/legacy-ctx ctx))]
              [stale/loading (stale/can-be-loading? ctx) (cats/fmap #(router/assoc-frag % (:frag props)) route')
               (fn [e] [error-comp e])
               ; legacy-ctx is set in build-link-props, and this is "downtree" from that
               (fn [route] [ui-from-route route ctx (css ?class (css-slugify (:link/rel link)))])])))])))
