(ns hyperfiddle.ide.fiddles.topnav
  (:require [cats.core :as cats]
            [cats.monad.either :as either]
            [cuerdas.core :as string]
            [hypercrud.browser.auto-fiddle :as auto-fiddle]
            [hypercrud.browser.base :as base]
            [hypercrud.browser.browser-ui :as browser-ui]
            [hypercrud.browser.context :as context]
            [hypercrud.browser.link :as link]
            [hypercrud.client.tx :as tx]
            [hypercrud.react.react-fragment :refer [react-fragment]]
            [hypercrud.ui.control.markdown-rendered :refer [markdown]]
            [hypercrud.ui.radio :as radio]
            [hypercrud.ui.result :as result]
            [hypercrud.ui.tooltip :refer [tooltip]]
            [hypercrud.util.core :as util :refer [unwrap]]
            [hypercrud.util.reactive :as reactive]
            [hyperfiddle.foundation :as foundation :refer [staging]]
            [hyperfiddle.foundation.actions :as foundation-actions]
            [hyperfiddle.ide.fiddles.topnav-bindings :as topnav-bindings]
            [hyperfiddle.runtime :as runtime]
            [reagent.core :as reagent]))


(defn stateless-login-url [ctx]
  (let [{:keys [domain client-id]} (get-in ctx [:hypercrud.browser/domain :domain/environment :auth0 (:hyperfiddle-hostname ctx)])
        callback-url (str "http://" (:hostname ctx) foundation/auth0-redirect-path)]
    (str domain "/login?client=" client-id "&callbackURL=" callback-url)))

; inline sys-link data when the entity is a system-fiddle
(letfn [(-shadow-fiddle [ctx fiddle]
          (let [[e a] (get-in ctx [:route :request-params])
                [_ fiddle-id] (:db/id e)]
            (if (auto-fiddle/system-fiddle? fiddle-id)     ; this fiddle does not actually exist, conjure it up
              (-> (unwrap (auto-fiddle/hydrate-system-fiddle fiddle-id))
                  (update :fiddle/bindings #(or (-> % meta :str) %))
                  (update :fiddle/renderer #(or (-> % meta :str) %))
                  (update :fiddle/request #(or (-> % meta :str) %)))
              fiddle)))]
  (defn shadow-fiddle [ctx]
    {:pre [(-> ctx :hypercrud.browser/result)]}
    (update ctx :hypercrud.browser/result (partial reactive/fmap (reactive/partial -shadow-fiddle ctx)))))

; ugly hacks to recursively fix the ui for sys links
(defn hijack-renderer [ctx]
  (let [ctx (dissoc ctx :user-renderer)
        f-mode-config (browser-ui/f-mode-config)
        ui-fn (-> (base/fn-from-mode f-mode-config (:hypercrud.browser/fiddle ctx) ctx)
                  (cats/mplus (either/right (:default f-mode-config)))
                  deref)]
    (ui-fn (shadow-fiddle ctx))))

(defn any-loading? [peer]
  (some (comp not nil? :hydrate-id val) @(runtime/state peer [::runtime/partitions])))

(defn loading-spinner [ctx]
  (if @(reactive/track any-loading? (:peer ctx))
    [:span {:style {:height "20px"
                    :width "23px"
                    :float "left"
                    :margin-right "1px"
                    :background-color "white"
                    :position "relative"
                    :z-index 0}}
     [:div {:style {:height "1em"
                    :margin-top "-2px"
                    :position "absolute"
                    :z-index -1}}
      [re-com.core/throbber :size :smaller]]]))

(defn -renderer [ctx]
  (let [ctx (unwrap (context/with-relations ctx))
        display-mode @(runtime/state (:peer ctx) [:display-mode])
        dirty? (not (empty? @(runtime/state (:peer ctx) [:stage])))
        ctx (shadow-fiddle ctx)
        ; hack until hyperfiddle.net#156 is complete
        fake-managed-anchor (fn [ident ctx label & args]
                              ; mostly copied from browser-ui
                              (let [kwargs (util/kwargs args)
                                    link (-> @(reactive/track link/rel->link ident ctx) (assoc :link/managed? true))
                                    props (-> (link/build-link-props link ctx true)
                                              #_(dissoc :style) #_"custom renderers don't want colored links")]
                                [(:navigate-cmp ctx) props label (:class kwargs)]))]
    [:div.hyperfiddle-topnav
     [:div.hyperfiddle-topnav-root-controls
      (fake-managed-anchor :domain ctx (get-in ctx [:target-domain :domain/ident]))
      " / "
      (let [ident @(reactive/cursor (:hypercrud.browser/result ctx) [:fiddle/ident])]
        (fake-managed-anchor :fiddle-more (assoc ctx :user-renderer hijack-renderer) (str ident)))
      " · "
      (fake-managed-anchor :links (assoc ctx :user-renderer hijack-renderer) "links")
      (fake-managed-anchor :ui (assoc ctx :user-renderer hijack-renderer) "view")
      (if @(runtime/state (:peer ctx) [::runtime/auto-transact])
        [:div
         [:input {:id ::auto-transact :type "checkbox" :checked true
                  :on-click (fn [] (runtime/dispatch! (:peer ctx) [:disable-auto-transact]))}]
         [:label {:for ::auto-transact} "auto-transact"]]
        (fake-managed-anchor :stage ctx "stage" :class (if dirty? "stage-dirty")))

      (let [change! #(runtime/dispatch! (:peer ctx) (foundation-actions/set-display-mode %))]
        [:span.radio-group
         (radio/option {:label "data" :tooltip "Edit data directly" :target :xray :value display-mode :change! change!})
         (radio/option {:label "view" :tooltip "View end-user UI" :target :user :value display-mode :change! change!})])

      [:div.right-nav {:key "right-nav"}                    ; CAREFUL; this key prevents popover flickering
       [loading-spinner ctx]
       ; ignore results; don't display the fiddle's data, just the anchors
       ((:anchor ctx) :new-fiddle ctx "new-fiddle")
       (if (:user-profile ctx)
         ((:anchor ctx) :account ctx (get-in ctx [:user-profile :email]))
         [:span.nav-link.auth [:a {:href (str (stateless-login-url ctx) "&state=" (runtime/encode-route (:peer ctx) (:target-route ctx)))} "Login"]])]]
     [:div.hyperfiddle-topnav-fiddle-controls
      ((:cell ctx) [true 0 :fiddle/type] ctx)
      ((:cell ctx) [true 0 :fiddle/pull] ctx)
      ((:cell ctx) [true 0 :fiddle/query] ctx)]]))

(defn- update-spacer [topnav]
  (let [measuredHeight (-> topnav (aget "fixed") .-offsetHeight)]
    (-> topnav (aget "spacer") (aget "style") (aset "height" (str measuredHeight "px")))))

(def renderer
  (reagent/create-class
    {:render (fn [this]
               [:div.hyperfiddle-topnav-container
                [:div.spacer {:ref (fn [!el] (aset this "spacer" !el))}]
                [:div.fixed {:ref (fn [!el] (aset this "fixed" !el))}
                 (let [[_ & args] (reagent/argv this)]
                   (apply vector -renderer args))]])
     :component-did-mount update-spacer
     :component-did-update update-spacer}))

(defn ^:export qe-picker-control [field props ctx]
  (let [enums [:query :entity :blank]
        change! #((:user-with! ctx) (tx/update-entity-attr @(:cell-data ctx) @(:hypercrud.browser/fat-attribute ctx) %))
        options (->> enums
                     (map #(radio/option
                             {:label (case % :query "query" :entity "pull" :blank "blank")
                              :target %
                              :value @(:value ctx)
                              :change! change!})))]
    [:span.qe.radio-group (apply react-fragment :_ options)]))

(defn ^:export stage-ui [ctx]
  (let [writes-allowed? (or (foundation/alias? (foundation/hostname->hf-domain-name ctx))
                            @(reactive/fmap (reactive/partial foundation/domain-owner? (:user-profile ctx))
                                            (runtime/state (:peer ctx) [::runtime/domain])))
        anonymous? (nil? (:user-profile ctx))
        stage @(runtime/state (:peer ctx) [:stage])]
    [:div.hyperfiddle-topnav-stage
     (result/view ctx)                                      ; for docstring
     (let [disabled? (or (not writes-allowed?) (not (empty? stage)))]
       [tooltip (cond (and (not writes-allowed?) anonymous?) {:status :warning :label "please login"}
                      (not writes-allowed?) {:status :warning :label "Writes restricted"}
                      (not (empty? stage)) {:status :warning :label "please transact! all changes first"})
        [:button {:disabled disabled?
                  :style (if disabled? {:pointer-events "none"})
                  :on-click (fn [] (runtime/dispatch! (:peer ctx) [:enable-auto-transact]))}
         "Enable auto-transact"]])
     (let [disabled? (or (not writes-allowed?) (empty? stage))]
       [tooltip (cond (and (not writes-allowed?) anonymous?) {:status :warning :label "please login"}
                      (not writes-allowed?) {:status :warning :label "Writes restricted"}
                      (empty? stage) {:status :warning :label "no changes"})
        [:button {:disabled disabled?
                  :style (if disabled? {:pointer-events "none"})
                  :on-click (fn []
                              ; specifically dont use the SplitRuntime protocol here. the only thing that makes sense is whats in the context from the route
                              (let [nil-branch-aux {:hyperfiddle.ide/foo "page"}]
                                (runtime/dispatch! (:peer ctx) (foundation-actions/manual-transact! (:peer ctx) (:hypercrud.browser/invert-route ctx) nil-branch-aux))))}
         "transact!"]])
     [staging (:peer ctx)]
     [:div.markdown [markdown "Hyperfiddle always generates valid transactions, if it doesn't, please file a bug.

*WARNING:* Datomic schema alterations cannot be used in the same transaction, for now you'll
need to transact the schema before using, see [#6](https://github.com/hyperfiddle/hyperfiddle/issues/6)."]]]))

(defn bindings [ctx] (topnav-bindings/bindings ctx))
