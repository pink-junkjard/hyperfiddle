(ns hyperfiddle.ui.iframe
  (:require
    [cats.core :refer [mlet]]
    [cats.monad.either :as either :refer [branch]]
    [contrib.css :refer [css css-slugify]]
    [contrib.data :refer [map-keys map-values]]
    [contrib.eval :as eval]
    [contrib.eval-cljs :as eval-cljs]
    [contrib.reactive :as r]
    [contrib.string :refer [blank->nil]]
    [contrib.ui.safe-render :refer [user-portal]]
    [hypercrud.browser.base :as base]
    [hypercrud.browser.context :as context]
    [hypercrud.browser.routing :as routing]
    [hypercrud.client.peer :as peer]
    [hypercrud.transit :as transit]
    [hypercrud.types.Err :as Err]
    [hypercrud.types.ThinEntity :refer [->ThinEntity]]
    [hypercrud.ui.error :as ui-error]
    [hypercrud.ui.stale :as stale]
    [hyperfiddle.fiddle :as fiddle]
    [hyperfiddle.runtime :as runtime]
    [reagent.core :as reagent]))


(defn- auto-ui-css-class [ctx]                              ; semantic css
  (let [ident @(r/cursor (:hypercrud.browser/fiddle ctx) [:fiddle/ident])]
    (->> ["hyperfiddle"
          (css-slugify (some-> ident namespace))
          (css-slugify ident)]
         (apply css))))

(defn- build-wrapped-render-expr-str [user-str] (str "(fn [val ctx props]\n" user-str ")"))

(def ^:private eval-renderer (memoize (fn [code-str & cache-bust] (eval/eval-expr-str!+ code-str))))

(defn- fiddle-renderer-cmp [value ctx props & bust-component-did-update]
  (let [last-cljs-ns (atom nil)]
    (fn [value ctx props & bust-component-did-update]
      (let [cljs-ns @(r/fmap-> (:hypercrud.browser/fiddle ctx) :fiddle/cljs-ns blank->nil)]
        (when (and (some? cljs-ns) (not= @last-cljs-ns cljs-ns))
          ; todo maybe use fiddle/ident for ns?
          (eval-cljs/eval-statement-str! 'user cljs-ns))
        (reset! last-cljs-ns cljs-ns)
        (-> @(r/cursor (:hypercrud.browser/fiddle ctx) [:fiddle/renderer])
            build-wrapped-render-expr-str
            (eval-renderer cljs-ns)
            (either/branch
              (fn [e] (throw e))
              (fn [f] [f value ctx props])))))))

(defn- ui-comp [ctx & [props]]                              ; user-renderer comes through here
  (let [value @(:hypercrud.browser/result ctx)              ; TODO remove this, make them ask
        props (update props :class css (auto-ui-css-class ctx))

        ; The reactdom component should filter the keys at the last second.
        ; https://github.com/hyperfiddle/hyperfiddle/issues/698
        view-props props #_(select-keys props [:class :initial-tab :on-click #_:disabled])
        display-mode @(:hypercrud.ui/display-mode ctx)
        error-props (-> (select-keys props [:class :on-click])
                        (update :class css "hyperfiddle-error"))]
    ^{:key (str display-mode)}
    [user-portal (ui-error/error-comp ctx) error-props
     (branch                                                 ; Validate context is well-formed and fiddles are valid.
       (context/valid+ ctx)
       (fn [e]
         [:pre (pr-str e #_(:hypercrud.browser/query-validation-issues ctx))])
       (fn [_]
         (case display-mode
           :hypercrud.browser.browser-ui/user (if-let [user-renderer (:user-renderer props)] ; validate qfind and stuff?
                                                [user-renderer value ctx view-props]
                                                ; If userland crashes (fiddle/renderer OR cljs-ns), reactions don't take hold, we need to reset here.
                                                ; Cheaper to pass this as a prop than to hash everything
                                                [fiddle-renderer-cmp value ctx view-props @(:hypercrud.browser/fiddle ctx)])
           :hypercrud.browser.browser-ui/xray [hyperfiddle.ui/fiddle-xray value ctx view-props]
           :hypercrud.browser.browser-ui/api [hyperfiddle.ui/fiddle-api value ctx view-props])))]))

(defn- fiddle-css-renderer [s] [:style {:dangerouslySetInnerHTML {:__html @s}}])

(defn- src-mode [route ctx]
  (mlet [ctx (-> (context/clean ctx)
                 #_(context/schemas (r/track context/summon-schemas-grouped-by-dbname ctx))
                 (routing/route+ route))
         request @(r/apply-inner-r (r/track base/meta-request-for-fiddle ctx))
         ctx (-> (context/source-mode ctx)
                 (context/clean)
                 (routing/route+ [nil [(->ThinEntity "$" [:fiddle/ident @(r/fmap first (:hypercrud.browser/route ctx))])]])
                 ; turns out we dont need fiddle for much if we already know the request
                 (context/schemas (r/track context/summon-schemas-grouped-by-dbname ctx))
                 (context/fiddle (r/pure {:fiddle/type :entity
                                          :fiddle/pull-database "$"})))]
    (base/process-results request ctx)))

(defn iframe-cmp [ctx {:keys [route hf-live] :as props}]    ; :: [route ctx & [?f props]]
  (let [click-fn (or (::on-click ctx) (constantly nil))     ; parent ctx receives click event, not child frame
        either-v (or (some-> @(runtime/state (:peer ctx) [::runtime/partitions (:branch ctx) :error]) either/left)
                     (if hf-live
                       (src-mode route ctx)
                       (base/data-from-route route ctx)))
        error-comp (ui-error/error-comp ctx)
        props (dissoc props :route)]
    [stale/loading (stale/can-be-loading? ctx) either-v
     (fn [e]
       (reagent/after-render
         (fn []
           (when (and (exists? js/Sentry)                   ; todo hide behind interface on runtime
                      (peer/loading? e)
                      (not @(stale/can-be-loading? ctx)))
             (.withScope js/Sentry (fn [scope]
                                     (.setExtra scope "ex-data" (clj->js (ex-data e)))
                                     (.setExtra scope "route" (pr-str route))
                                     (.setExtra scope "global-basis" (->> @(runtime/state (:peer ctx) [::runtime/global-basis])
                                                                          (map-values #(map-keys str %))
                                                                          (clj->js)))
                                     (.setExtra scope "branch-ident" (clj->js (:branch ctx)))
                                     (.setExtra scope "branch-state" (-> @(runtime/state (:peer ctx) [::runtime/partitions (:branch ctx)])
                                                                         (select-keys [:route :local-basis :ptm])
                                                                         (update :ptm keys)
                                                                         (transit/encode)))
                                     (.captureMessage js/Sentry (str (cond
                                                                       (Err/Err? e) (:msg e)
                                                                       (map? e) (:message e)
                                                                       (string? e) e
                                                                       :else (ex-message e)))))))))
       [error-comp e {:class (css "hyperfiddle-error" (:class props) "ui")
                      :on-click (r/partial click-fn route)}])
     (fn [ctx]                                              ; fresh clean ctx
       [:<>
        [ui-comp ctx (-> (update props :class css "ui")
                         ; @route here is a broken reaction, see routing/route+ returns a severed reaciton
                         (assoc :on-click (r/partial click-fn @(:hypercrud.browser/route ctx))))]
        [fiddle-css-renderer (r/cursor (:hypercrud.browser/fiddle ctx) [:fiddle/css])]])
     (fn [ctx]
       [:<>
        [ui-comp ctx (-> (update props :class css "hyperfiddle-loading" "ui")
                         ; use the stale ctx's route, otherwise alt clicking while loading could take you to the new route, which is jarring
                         (assoc :on-click (r/partial click-fn @(:hypercrud.browser/route ctx))))]
        [fiddle-css-renderer (r/cursor (:hypercrud.browser/fiddle ctx) [:fiddle/css])]])]))
