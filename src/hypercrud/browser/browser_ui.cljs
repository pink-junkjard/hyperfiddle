(ns hypercrud.browser.browser-ui
  (:require [cats.core :as cats :refer [mlet >>=]]
            [cats.monad.either :as either]
            [contrib.css :refer [css-slugify classes]]
            [contrib.data :refer [cond-let map-values unwrap kwargs]]
            [contrib.eval :as eval]
            [contrib.pprint :refer [pprint-str]]
            [contrib.reactive :as r]
            [contrib.string :refer [memoized-safe-read-edn-string blank->nil]]
            [contrib.try :refer [try-either]]
            [cuerdas.core :as string]
            [hypercrud.browser.base :as base]
            [hypercrud.browser.context :as context]
            [hypercrud.browser.link :as link]
            [hypercrud.browser.routing :as routing]
            [hypercrud.types.ThinEntity :refer [->ThinEntity]]
            [hypercrud.ui.error :as ui-error]
    ; [hypercrud.ui.form :as form]
            [contrib.ui.native-event-listener :refer [native-on-click-listener]]
            [hypercrud.ui.safe-render :refer [user-portal]]
            [hypercrud.ui.stale :as stale]
            [hyperfiddle.foundation :as foundation]
            [hyperfiddle.foundation.actions :as foundation-actions]
            [hyperfiddle.runtime :as runtime]))


(declare ui-from-link)

(defn fiddle-css-renderer [s]
  [:style {:dangerouslySetInnerHTML {:__html @s}}])

(defn auto-ui-css-class [ctx]
  (classes (let [ident @(r/cursor (:hypercrud.browser/fiddle ctx) [:fiddle/ident])]
             [(css-slugify (some-> ident namespace))
              (css-slugify ident)
              "auto-result"])))

(letfn [(browse [rel #_dependent? path ctx ?f & args]
          (let [props (kwargs args)
                {:keys [:link/dependent? :link/path] :as link} @(r/track link/rel->link rel path ctx)
                ctx (-> (context/relation-path ctx (into [dependent?] (unwrap (memoized-safe-read-edn-string (str "[" path "]")))))
                        (as-> ctx (if ?f (assoc ctx :user-renderer ?f #_(if ?f #(apply ?f %1 %2 %3 %4 args))) ctx)))]
            (into [ui-from-link link ctx (:class props)] (apply concat (dissoc props :class :children nil)))))
        (anchor [rel #_dependent? path ctx label & args]
          (let [{:keys [:link/dependent? :link/path] :as link} @(r/track link/rel->link rel path ctx)
                ctx (context/relation-path ctx (into [dependent?] (unwrap (memoized-safe-read-edn-string (str "[" path "]")))))
                props (kwargs args)]
            [(:navigate-cmp ctx) (merge props (link/build-link-props link ctx)) label (:class props)]))
        (field [[d i a] ctx ?f & args]
          (let [cell (case (:layout ctx) :table hypercrud.ui.table/Field hypercrud.ui.form/Field)]
            [(r/partial cell ?f)          ; Intentional explicit nil
             (context/relation-path ctx [d i a])
             (kwargs args)]))
        (value [path ctx ?f & args]
          (let [ctx (context/relation-path ctx path)]
            [(or ?f (hypercrud.ui.auto-control/auto-control ctx)) @(:value ctx) ctx (kwargs args)]))
        (browse' [rel #_dependent? path ctx]
          (->> (base/data-from-link @(r/track link/rel->link rel path ctx) ctx)
               (cats/fmap :hypercrud.browser/result)
               (cats/fmap deref)))]
  ; convenience functions, should be declared fns in this or another ns and accessed out of band of ctx
  (defn ui-bindings [ctx]
    (assoc ctx
      :anchor anchor
      :browse browse
      :cell field
      :value value
      :browse' browse')))

(defn page-on-click [rt branch branch-aux route event]
  (when (and route (.-altKey event))
    (runtime/dispatch! rt (fn [dispatch! get-state]
                            (when (foundation/navigable? route (get-state))
                              (foundation-actions/set-route rt route branch false false dispatch! get-state))))
    (.stopPropagation event)))

; defer eval until render cycle inside userportal
(let [safe-eval-string #(try-either (when % (eval/eval-string %))) ; don't actually need to safely eval, just want to memoize exceptions
      memoized-eval-string (memoize safe-eval-string)]
  (defn eval-renderer-comp [?fiddle-cljs-ns-str fiddle-renderer-str & args]
    (let [result (>>= (memoized-eval-string ?fiddle-cljs-ns-str)
                      (constantly
                        ; eval ns for the effect on the cljs namespaces
                        (memoized-eval-string fiddle-renderer-str)))]
      (either/branch
        result
        (fn [e]
          (throw e))
        (fn [f]
          (into [f] args))))))

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
      (let [f (or (:user-renderer ctx) hyperfiddle.ide.fiddles.fiddle-src/fiddle-src-renderer)]
        [f ctx (auto-ui-css-class ctx) :embed-mode true]))))

(defn ui-comp [ctx]
  [user-portal (ui-error/error-comp ctx)
   (if (hyperfiddle.ide.fiddles.topnav/src-mode? (get (:route ctx) 3))
     [src-mode ctx]
     (let [class (auto-ui-css-class ctx)]
       (case @(:hypercrud.ui/display-mode ctx)
         :user (if-let [user-renderer (:user-renderer ctx)]
                 [user-renderer ctx class]
                 [eval-renderer-comp
                  (some-> @(r/cursor (:hypercrud.browser/fiddle ctx) [:fiddle/cljs-ns]) blank->nil)
                  (some-> @(r/cursor (:hypercrud.browser/fiddle ctx) [:fiddle/renderer]) blank->nil build-wrapped-render-expr-str)
                  ctx class])
         ; todo ui.result should be injected
         :xray [hypercrud.ui.result/fiddle-xray ctx class]
         :edn [hypercrud.ui.result/fiddle-edn ctx class])))])

(defn ui-from-route [route ctx & [class]]
  (let [click-fn (or (:hypercrud.browser/page-on-click ctx) (constantly nil)) ; parent ctx receives click event, not child frame
        either-v (->> (or (some-> @(runtime/state (:peer ctx) [::runtime/partitions (:branch ctx) :error]) either/left)
                          (base/data-from-route route ctx))
                      (cats/fmap ui-bindings))
        error-comp (ui-error/error-comp ctx)]
    [stale/loading (stale/can-be-loading? ctx) either-v
     (fn [e]
       (let [on-click (r/partial click-fn route)]
         [native-on-click-listener {:on-click on-click}
          [:div {:class (classes "ui" class "hyperfiddle-error")}
           [error-comp e]]]))
     (fn [ctx]
       (let [on-click (r/partial click-fn (:route ctx))]
         [native-on-click-listener {:on-click on-click}
          [:div {:class (classes "ui" class)}
           [ui-comp ctx]
           [fiddle-css-renderer (r/cursor (:hypercrud.browser/fiddle ctx) [:fiddle/css])]]]))
     (fn [ctx]
       (let [on-click (r/partial click-fn (:route ctx))]
         ; use the stale ctx's route, otherwise alt clicking while loading could take you to the new route, which is jarring
         [native-on-click-listener {:on-click on-click}
          [:div {:class (classes "ui" class "hyperfiddle-loading")}
           [ui-comp ctx]
           [fiddle-css-renderer (r/cursor (:hypercrud.browser/fiddle ctx) [:fiddle/css])]]]))]))

(defn ui-from-link [link ctx ?class & args]
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
          (fn [route] [ui-from-route route ctx (classes ?class (css-slugify (:link/rel link)))])]))]))
