(ns hypercrud.browser.browser-ui
  (:require
    [cats.core :refer [mlet]]
    [cats.monad.either :as either]
    [contrib.css :refer [css-slugify classes]]
    [contrib.reactive :as r]
    [contrib.string :refer [blank->nil]]
    [hypercrud.browser.base :as base]
    [hypercrud.browser.context :as context]
    [hypercrud.types.ThinEntity :refer [->ThinEntity]]
    [hypercrud.ui.error :as ui-error]
    [hypercrud.ui.safe-render :refer [user-portal]]
    [hypercrud.ui.util :as util]
    [hyperfiddle.actions :as actions]
    [hyperfiddle.foundation :as foundation]
    [hyperfiddle.ide.fiddles.fiddle-src :as fiddle-src]
    [hyperfiddle.ide.fiddles.topnav :as topnav]
    [hyperfiddle.ui :refer [fiddle-xray]]
    [hyperfiddle.runtime :as runtime]))


(defn auto-ui-css-class [ctx]
  (classes (let [ident @(r/cursor (:hypercrud.browser/fiddle ctx) [:fiddle/ident])]
             [(css-slugify (some-> ident namespace))
              (css-slugify ident)
              "auto-result"])))

(defn page-on-click [rt branch branch-aux route event]
  (when (and route (.-altKey event))
    (runtime/dispatch! rt (fn [dispatch! get-state]
                            (when (foundation/navigable? route (get-state))
                              (actions/set-route rt route branch false false dispatch! get-state))))
    (.stopPropagation event)))

(defn build-wrapped-render-expr-str [user-str] (str "(fn [ctx & [class]]\n" user-str ")"))

(defn src-mode [ctx]
  (either/branch
    (mlet [request @(r/apply-inner-r (r/track base/meta-request-for-fiddle ctx))
           :let [fiddle (r/atom {:fiddle/type :entity
                                 :fiddle/pull-database "$"}) ; turns out we dont need fiddle for much if we already know the request
                 ctx (-> (context/source-mode ctx)
                         (context/clean)
                         (context/route [nil [(->ThinEntity "$" [:fiddle/ident (first (:route ctx))])]]))]]
      (base/process-results fiddle request ctx))
    (fn [e] (throw e))                                      ; just throw, this is inside a user-portal
    (fn [ctx]
      (let [f (or (:user-renderer ctx) fiddle-src/fiddle-src-renderer)]
        [f ctx (auto-ui-css-class ctx) :embed-mode true]))))

(defn ui-comp [ctx]
  [user-portal (ui-error/error-comp ctx)
   (if (topnav/src-mode? (get (:route ctx) 3))
     [src-mode ctx]
     (let [class (auto-ui-css-class ctx)]
       (case @(:hypercrud.ui/display-mode ctx)
         ::user (if-let [user-renderer (:user-renderer ctx)]
                  [user-renderer ctx class]
                  [util/eval-renderer-comp
                   (some-> @(r/cursor (:hypercrud.browser/fiddle ctx) [:fiddle/cljs-ns]) blank->nil)
                   (some-> @(r/cursor (:hypercrud.browser/fiddle ctx) [:fiddle/renderer]) blank->nil build-wrapped-render-expr-str)
                   ctx class])
         ::xray [fiddle-xray ctx class])))])
