(ns hypercrud.browser.browser-ui
  (:require
    [cats.core :refer [mlet]]
    [cats.monad.either :as either]
    [contrib.css :refer [css-slugify css]]
    [contrib.reactive :as r]
    [contrib.reagent-native-events :refer [button=]]
    [contrib.rfc3986 :refer [encode-rfc3986-pchar encode-ednish]]
    [contrib.string :refer [blank->nil]]
    [contrib.ui.safe-render :refer [user-portal]]
    [hypercrud.browser.base :as base]
    [hypercrud.browser.context :as context]
    [hypercrud.browser.router :as router]
    [hypercrud.browser.routing :as routing]
    [hypercrud.types.ThinEntity :refer [->ThinEntity]]
    [hypercrud.ui.error :as ui-error]
    [hyperfiddle.ide.fiddles.fiddle-src :as fiddle-src]
    [hyperfiddle.ide.fiddles.topnav :as topnav]
    [hyperfiddle.ui :refer [fiddle-xray]]
    [hyperfiddle.ui.util :as util]))


(defn auto-ui-css-class [ctx]
  (css (let [ident @(r/cursor (:hypercrud.browser/fiddle ctx) [:fiddle/ident])]
         ["hyperfiddle"
          (css-slugify (some-> ident namespace))
          (css-slugify ident)])))

(defn page-on-click "middle-click will open in new tab; alt-middle-click will open srcmode in new tab"
  [rt branch branch-aux route event]
  (when route
    (let [anchor (-> event .-path (aget 0) (.matches "a"))
          anchor-descendant (-> event .-path (aget 0) (.matches "a *"))]
      (when-not (or anchor anchor-descendant)
        (let [open-new-tab (or (button= :middle event)
                               (and (button= :left event)
                                    (.-metaKey event)))]
          (cond
            open-new-tab (let [target-src (.-altKey event)
                               route (if target-src
                                       (router/assoc-frag route (encode-rfc3986-pchar (encode-ednish (pr-str :src))))
                                       route)]
                           (.stopPropagation event)
                           (js/window.open (router/encode route) "_blank"))

            ; Do we even need to drill down into this tab under any circumstances? I suspect not.
            ; Can always middle click and then close this tab.
            #_#_:else (runtime/dispatch! rt (fn [dispatch! get-state]
                                              (when (foundation/navigable? route (get-state))
                                                (actions/set-route rt route branch false false dispatch! get-state))))))))))

(defn build-wrapped-render-expr-str [user-str] (str "(fn [ctx & [class]]\n" user-str ")"))

(defn src-mode [ctx & [?class]]
  (either/branch
    (mlet [request @(r/apply-inner-r (r/track base/meta-request-for-fiddle ctx))
           :let [fiddle (r/atom {:fiddle/type :entity
                                 :fiddle/pull-database "$"}) ; turns out we dont need fiddle for much if we already know the request
                 ctx (-> (context/source-mode ctx)
                         (context/clean)
                         (routing/route [nil [(->ThinEntity "$" [:fiddle/ident (first (:route ctx))])]]))]]
      (base/process-results fiddle request ctx))
    (fn [e] (throw e))                                      ; just throw, this is inside a user-portal
    (fn [ctx]
      (let [f (or (:user-renderer ctx) fiddle-src/fiddle-src-renderer)]
        [f ctx (css ?class (auto-ui-css-class ctx)) :embed-mode true]))))

(defn ui-comp [ctx & [?class]]
  [user-portal (ui-error/error-comp ctx)
   (if (topnav/src-mode? (get (:route ctx) 3))
     [src-mode ctx ?class]
     (let [class (css ?class (auto-ui-css-class ctx))]
       (case @(:hypercrud.ui/display-mode ctx)
         ::user (if-let [user-renderer (:user-renderer ctx)]
                  [user-renderer ctx class]
                  [util/eval-renderer-comp
                   (some-> @(r/cursor (:hypercrud.browser/fiddle ctx) [:fiddle/cljs-ns]) blank->nil)
                   (some-> @(r/cursor (:hypercrud.browser/fiddle ctx) [:fiddle/renderer]) blank->nil build-wrapped-render-expr-str)
                   ctx class])
         ::xray [fiddle-xray ctx class])))])
