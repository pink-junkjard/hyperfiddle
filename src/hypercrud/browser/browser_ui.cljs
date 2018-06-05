(ns hypercrud.browser.browser-ui
  (:require
    [cats.core :as cats :refer [mlet]]
    [cats.monad.either :as either]
    [contrib.css :refer [css-slugify classes]]
    [contrib.data :refer [unwrap kwargs]]
    [contrib.reactive :as r]
    [contrib.string :refer [memoized-safe-read-edn-string blank->nil]]
    [hypercrud.browser.base :as base]
    [hypercrud.browser.context :as context]
    [hypercrud.browser.core :as browser]
    [hypercrud.browser.link :as link]
    [hypercrud.types.ThinEntity :refer [->ThinEntity]]
    [hypercrud.ui.auto-control :as auto-control]
    [hypercrud.ui.error :as ui-error]
    [hypercrud.ui.form :as form]
    [hypercrud.ui.result :as result]
    [hypercrud.ui.safe-render :refer [user-portal]]
    [hypercrud.ui.table :as table]
    [hypercrud.ui.util :as util]
    [hyperfiddle.actions :as actions]
    [hyperfiddle.foundation :as foundation]
    [hyperfiddle.ide.fiddles.fiddle-src :as fiddle-src]
    [hyperfiddle.ide.fiddles.topnav :as topnav]
    [hyperfiddle.runtime :as runtime]))


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
            (into [browser/ui link ctx (:class props)] (apply concat (dissoc props :class :children nil)))))
        (anchor [rel #_dependent? path ctx label & args]
          (let [{:keys [:link/dependent? :link/path] :as link} @(r/track link/rel->link rel path ctx)
                ctx (context/relation-path ctx (into [dependent?] (unwrap (memoized-safe-read-edn-string (str "[" path "]")))))
                props (kwargs args)]
            [(:navigate-cmp ctx) (merge props (link/build-link-props link ctx)) label (:class props)]))
        (field [[i a] ctx ?f & args]
          (let [cell (case (:hyperfiddle.ui/layout ctx) :hyperfiddle.ui.layout/table table/Field form/Field)]
            [(r/partial cell ?f)                            ; Intentional explicit nil
             (context/relation-path ctx [true i a])
             (kwargs args)]))
        (value [[i a] ctx ?f & args]
          (let [ctx (context/relation-path ctx [true i a])]
            [(or ?f (auto-control/auto-control ctx)) @(:value ctx) ctx (kwargs args)]))
        (browse' [rel #_dependent? path ctx]
          (->> (base/data-from-link @(r/track link/rel->link rel path ctx) ctx)
               (cats/fmap :hypercrud.browser/result)
               (cats/fmap deref)))]
  ; convenience functions, should be declared fns in this or another ns and accessed out of band of ctx
  (defn ui-bindings [ctx]
    (assoc ctx
      :anchor anchor
      :browse browse
      :field field
      :cell field                                           ; legacy
      :value value
      :browse' browse')))

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
         ::xray [result/fiddle-xray ctx class])))])
