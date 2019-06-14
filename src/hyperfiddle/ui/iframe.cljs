(ns hyperfiddle.ui.iframe
  (:require
    [cats.monad.either :as either]
    [contrib.css :refer [css css-slugify]]
    [contrib.eval :as eval]
    [contrib.eval-cljs :as eval-cljs]
    [contrib.reactive :as r]
    [contrib.string :refer [blank->nil]]
    [contrib.ui.safe-render :refer [user-portal]]
    [hypercrud.browser.base :as base]
    [hypercrud.ui.error :as ui-error]
    [hypercrud.ui.stale :as stale]
    [hyperfiddle.runtime :as runtime]))


(defn auto-ui-css-class [?ctx]                              ; semantic css
  (let [ident (some-> (:hypercrud.browser/fiddle ?ctx) (r/cursor [:fiddle/ident]) deref)]
    (->> ["hyperfiddle"
          (css-slugify (some-> ident namespace))
          (css-slugify ident)]
         (apply css))))

(defn- build-wrapped-render-expr-str [user-str] (str "(fn [val ctx props]\n" user-str ")"))

(def ^:private eval-renderer (memoize (fn [code-str & cache-bust] (eval/eval-expr-str!+ code-str))))

(defn- fiddle-renderer-cmp [value ctx props & bust-component-did-update]
  (let [last-cljs-ns (atom nil)]                            ; don't spam the compiler - memoize buffer of 1
    (fn [value ctx props & bust-component-did-update]
      (let [cljs-ns @(r/fmap-> (:hypercrud.browser/fiddle ctx) :fiddle/cljs-ns blank->nil)] ; always do this
        (when (and (some? cljs-ns)
                   (not= @last-cljs-ns cljs-ns))
          (eval-cljs/eval-statement-str! 'user cljs-ns))    ; Exceptions caught at user-portal error-comp
        (reset! last-cljs-ns cljs-ns)

        (let [display-mode (or (some-> (:hypercrud.ui/display-mode ctx) deref)
                               :hypercrud.browser.browser-ui/user)
              props props #_(select-keys props [:class :initial-tab :on-click #_:disabled])]
          (case display-mode

            :hypercrud.browser.browser-ui/user
            (if-let [user-renderer (:user-renderer props)]  ; validate qfind and stuff?
              [user-renderer value ctx props]
              (-> @(r/cursor (:hypercrud.browser/fiddle ctx) [:fiddle/renderer])
                  build-wrapped-render-expr-str
                  (eval-renderer cljs-ns)
                  (either/branch
                    (fn [e] (throw e))
                    (fn [f] [f value ctx props]))))

            :hypercrud.browser.browser-ui/xray
            (if-let [user-renderer (:user-renderer props)]
              ; don't use r/partial with user-renderer, r/partial useful for args, not components
              ; Select user-renderer is valid in xray mode now
              [user-renderer value ctx props]
              [hyperfiddle.ui/fiddle-xray value ctx props])

            :hypercrud.browser.browser-ui/api
            [hyperfiddle.ui/fiddle-api value ctx props]))))))

(defn- ui-comp [ctx & [props]]                              ; user-renderer comes through here
  (let [value @(:hypercrud.browser/result ctx)              ; TODO remove this, make them ask
        props (update props :class css (auto-ui-css-class ctx))
        ; The reactdom component should filter the keys at the last second.
        ; https://github.com/hyperfiddle/hyperfiddle/issues/698
        display-mode (or (some-> (:hypercrud.ui/display-mode ctx) deref) :hypercrud.browser.browser-ui/user)
        error-props (-> (select-keys props [:class :on-click])
                        (update :class css "hyperfiddle-error"))]
    ^{:key (str display-mode)}
    [user-portal (ui-error/error-comp ctx) error-props
     ; If userland crashes (fiddle/renderer OR cljs-ns), reactions don't take hold, we need to reset here.
     ; Cheaper to pass fiddle-value as a prop than to hash everything
     [fiddle-renderer-cmp value ctx props @(:hypercrud.browser/fiddle ctx)]]))

(defn- fiddle-css-renderer [s] [:style {:dangerouslySetInnerHTML {:__html @s}}])

(defn iframe-cmp [ctx {:keys [route] :as props}]            ; :: [route ctx & [?f props]]
  (let [either-v (or (some-> @(runtime/state (:peer ctx) [::runtime/partitions (:branch ctx) :error]) either/left) ; todo this partition-error check should happen higher
                     (base/browse-route+ route ctx))
        error-comp (or (:hyperfiddle.ui/error-render-custom props) ; try props first
                       (ui-error/error-comp ctx))
        props (dissoc props :route :hyperfiddle.ui/error-render-custom)]
    [stale/loading (r/track runtime/branch-is-loading? (:peer ctx) (:branch ctx)) either-v
     (fn [e]
       [error-comp e (cond-> {:class (css "hyperfiddle-error" (:class props) "ui")}
                       (::on-click ctx) (assoc :on-click (r/partial (::on-click ctx) route)))])
     (fn [ctx]                                              ; fresh clean ctx
       [:<>
        [ui-comp ctx (cond-> (update props :class css "ui")
                       ; @route here is a broken reaction, see routing/route+ returns a severed reaciton
                       (::on-click ctx) (assoc :on-click (r/partial (::on-click ctx) @(:hypercrud.browser/route ctx))))]
        [fiddle-css-renderer (r/cursor (:hypercrud.browser/fiddle ctx) [:fiddle/css])]])
     (fn [ctx]
       [:<>
        [ui-comp ctx (cond-> (update props :class css "hyperfiddle-loading" "ui")
                       ; use the stale ctx's route, otherwise alt clicking while loading could take you to the new route, which is jarring
                       (::on-click ctx) (assoc :on-click (r/partial (::on-click ctx) @(:hypercrud.browser/route ctx))))]
        [fiddle-css-renderer (r/cursor (:hypercrud.browser/fiddle ctx) [:fiddle/css])]])]))
