(ns hyperfiddle.ui.iframe
  (:require
    [cats.monad.either :as either]
    [clojure.spec.alpha :as s]
    [contrib.css :refer [css css-slugify]]
    [contrib.eval :as eval]
    [contrib.eval-cljs :as eval-cljs]
    [contrib.pprint :refer [pprint-str]]
    [contrib.reactive :as r]
    [contrib.reader :as reader]
    [contrib.string :refer [blank->nil]]
    [contrib.ui]
    [contrib.ui.safe-render :refer [user-portal]]
    [hypercrud.browser.base :as base]
    [hypercrud.browser.context :as context]
    [hyperfiddle.branch :as branch]
    [hyperfiddle.runtime :as runtime]
    [hyperfiddle.ui.error :as ui-error]
    [hyperfiddle.ui.stale :as stale]
    [taoensso.timbre :as timbre]))


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

        (let [display-mode (or (some-> (:hyperfiddle.ui/display-mode ctx) deref)
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
        display-mode (or (some-> (:hyperfiddle.ui/display-mode ctx) deref) :hypercrud.browser.browser-ui/user)
        error-props (-> (select-keys props [:class :on-click])
                        (update :class css "hyperfiddle-error"))]
    ^{:key (str display-mode)}
    [user-portal (ui-error/error-comp ctx) error-props
     ; If userland crashes (fiddle/renderer OR cljs-ns), reactions don't take hold, we need to reset here.
     ; Cheaper to pass fiddle-value as a prop than to hash everything
     [fiddle-renderer-cmp value ctx props @(:hypercrud.browser/fiddle ctx)]]))

(defn- fiddle-css-renderer [s] [:style {:dangerouslySetInnerHTML {:__html @s}}])

(defn route-editor [route on-change]
  (let [parse-string (fn [s]
                       (let [route (reader/read-edn-string! s)]
                         (s/assert :hyperfiddle/route route)
                         route))
        to-string pprint-str]
    [contrib.ui/debounced
     {:value route
      :debounce/interval 500
      :on-change (fn [o n] (on-change n))
      :mode "clojure"
      :lineNumbers false}
     contrib.ui/validated-cmp parse-string to-string contrib.ui/code]))

(defn stale-browse
  ([route ctx error success] (stale-browse route ctx error success success))
  ([route ctx error success loading]
   [stale/loading
    (r/track runtime/branch-is-loading? (:peer ctx) (:branch ctx))
    (base/browse-route+ ctx route)
    (fn [e] [error e])
    (fn [ctx] [success ctx])
    (fn [ctx] [loading ctx])]))

(defn iframe-cmp-impl [route route-on-change ctx & [props]]
  (let [error-comp (or (:hyperfiddle.ui/error-render-custom props) ; try props first
                       (ui-error/error-comp ctx))
        ctx (assoc ctx :hyperfiddle.ui/-set-route route-on-change) ; todo runtime/set-route
        props (dissoc props :hyperfiddle.ui/error-render-custom)]
    [:<>
     (when-not (= :hypercrud.browser.browser-ui/user (or (some-> (:hyperfiddle.ui/display-mode ctx) deref) :hypercrud.browser.browser-ui/user))
       [route-editor route route-on-change])
     [stale-browse route ctx
      (fn [e]
        [error-comp e (cond-> {:class (css "hyperfiddle-error" (:class props) "ui")}
                        (::on-click ctx) (assoc :on-click (r/partial (::on-click ctx) route)))])
      (fn [ctx]                                             ; fresh clean ctx
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
         [fiddle-css-renderer (r/cursor (:hypercrud.browser/fiddle ctx) [:fiddle/css])]])]]))

(defn managed-route-editor-state [display-cmp route {rt :peer :as ctx} {:keys [::relative-child-branch-id] :as props}]
  (let [local-state (r/atom {:initial route :current route})
        relative-branch-id (or relative-child-branch-id (context/build-child-branch-relative-id ctx (context/eav ctx)))
        child-branch-id (branch/child-branch-id (:branch ctx) relative-branch-id)
        on-change (fn [new-route]
                    (if (= new-route (:initial @local-state))
                      (when (runtime/branch-exists? rt child-branch-id)
                        (runtime/discard-branch rt child-branch-id))
                      (do
                        (when-not (runtime/branch-exists? rt child-branch-id)
                          (runtime/create-branch rt child-branch-id))
                        (runtime/set-route rt child-branch-id new-route)))
                    (swap! local-state assoc :current new-route))]
    (fn [display-cmp route ctx props]
      (when-not (= (:initial @local-state) route)
        (reset! local-state {:initial route :current route}))
      (let [local-route (:current @local-state)
            props (dissoc props ::relative-child-branch-id)]
        (-> (if (or (= route local-route))
              (either/right ctx)
              (context/branch+ ctx relative-branch-id))
            (either/branch
              (fn [e] [(ui-error/error-comp ctx) e])
              (fn [ctx] [display-cmp local-route on-change ctx props])))))))

(defn iframe-cmp
  ([ctx props]
   (timbre/warn "Deprecated arity of hyperfiddle.ui.iframe/iframe-cmp. Explicitly provide route argument")
   [iframe-cmp (:route props) ctx (dissoc props :route)])
  ([route ctx props] [managed-route-editor-state iframe-cmp-impl route ctx props]))
