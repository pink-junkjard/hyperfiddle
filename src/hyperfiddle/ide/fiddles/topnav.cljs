(ns hyperfiddle.ide.fiddles.topnav
  (:require [cats.core :refer [fmap]]
            [contrib.base-64-url-safe :as base64-url-safe]
            [contrib.data :refer [kwargs unwrap]]
            [contrib.datomic-tx :as tx]
            [contrib.reactive :as r]
            [contrib.reader :refer [read-edn-string]]
            [contrib.reagent :refer [fragment]]
            [contrib.rfc3986 :refer [encode-ednish encode-rfc3986-pchar]]
            [hypercrud.browser.context :as context]
            [hypercrud.browser.fiddle :as fiddle]
            [hypercrud.browser.link :as link]
            [hypercrud.browser.router :as router]
            [hypercrud.browser.system-fiddle :as system-fiddle]
            [hypercrud.types.Entity :refer [->Entity shadow-entity]]
            [hyperfiddle.ui :refer [markdown]]
            [contrib.ui.radio :as radio]
            [hypercrud.ui.result :as result]
            [contrib.ui.tooltip :refer [tooltip]]
            [hyperfiddle.foundation :as foundation :refer [staging]]
            [hyperfiddle.foundation.actions :as foundation-actions]
            [hyperfiddle.runtime :as runtime]))


(defn stateless-login-url [ctx]
  (let [{:keys [domain client-id]} (get-in ctx [:hypercrud.browser/domain :domain/environment :auth0 (:hyperfiddle-hostname ctx)])
        callback-url (str "http://" (:hostname ctx) foundation/auth0-redirect-path)]
    (str domain "/login?client=" client-id "&callbackURL=" callback-url)))

; inline sys-link data when the entity is a system-fiddle
(letfn [(-shadow-fiddle [target-ident fiddle-val]
          (cond
            (system-fiddle/system-fiddle? target-ident) (->> (system-fiddle/hydrate-system-fiddle target-ident)
                                                             (fmap fiddle/fiddle-defaults)
                                                             unwrap
                                                             (->Entity nil))

            (nil? (:db/id fiddle-val)) fiddle-val
            :else (shadow-entity fiddle-val fiddle/fiddle-defaults)))]
  (defn shadow-fiddle [ctx]
    {:pre [(-> ctx :hypercrud.browser/result)]}
    (let [route (:route ctx)
          [_ [e]] route                                     ; [:hyperfiddle/topnav [#entity["$" [:fiddle/ident :hyperfiddle.system/remove]]]]
          [_ target-fiddle-ident] (:db/id e)]
      (-> ctx
          (dissoc :relation :relations)
          (update :hypercrud.browser/result (partial r/fmap (r/partial -shadow-fiddle target-fiddle-ident)))
          (context/with-relations)))))

(defn any-loading? [peer]
  (some (comp not nil? :hydrate-id val) @(runtime/state peer [::runtime/partitions])))

(defn loading-spinner [ctx & [?class]]
  (if @(r/track any-loading? (:peer ctx))
    [:div.display-inline-flex [re-com.core/throbber]]))

(defn src-mode? [frag]
  (= :src (some-> frag read-edn-string)))

(defn renderer [ctx class]
  {:pre [(or (:relations ctx) (:relation ctx))]}
  (let [display-mode @(runtime/state (:peer ctx) [:display-mode])
        dirty? (not (empty? @(runtime/state (:peer ctx) [:stage])))
        ctx (shadow-fiddle ctx)
        ; hack until hyperfiddle.net#156 is complete
        fake-managed-anchor (fn [rel path ctx label & args]
                              ; mostly copied from browser-ui
                              (let [kwargs (kwargs args)
                                    link (-> @(r/track link/rel->link rel path ctx) (assoc :link/managed? true))
                                    props (-> (link/build-link-props link ctx true)
                                              #_(dissoc :style) #_"custom renderers don't want colored links")]
                                [(:navigate-cmp ctx) props label (:class kwargs)]))]
    [:div {:class class}
     [:div.left-nav
      [tooltip {:label nil}
       (fake-managed-anchor :domain [] ctx (get-in ctx [:target-domain :domain/ident]))]
      [tooltip {:label "Fiddle ident"}
       [:div (some-> @(r/cursor (:hypercrud.browser/result ctx) [:fiddle/ident]) str)]]]

     [:div.right-nav {:key "right-nav"}                     ; CAREFUL; this key prevents popover flickering

      [loading-spinner ctx]

      (let [src-mode (src-mode? (-> ctx :target-route (get 3)))
            change! #(runtime/dispatch! (:peer ctx) (foundation-actions/set-display-mode %))]
        [:span.radio-group
         (radio/option {:label "data" :tooltip "Ignore :fiddle/renderer" :target :xray :change! change! :value (if src-mode :src display-mode) :disabled src-mode})
         (radio/option {:label "view" :tooltip "Use :fiddle/renderer" :target :user :value (if src-mode :src display-mode) :change! change! :disabled src-mode})
         (radio/option {:label (let [root-rel-path (runtime/encode-route (:peer ctx) (router/dissoc-frag (:target-route ctx)))
                                     href (if-not src-mode
                                            (str root-rel-path "#" (encode-rfc3986-pchar (encode-ednish (pr-str :src))))
                                            (str root-rel-path "#"))]
                                 (if-not src-mode
                                   [:a {:href href :target "_blank"} "src"]
                                   [:span "src"]))
                        :tooltip "View fiddle source" :target :src :value (if src-mode :src display-mode) :change! change! :disabled (not src-mode)})])

      (if @(runtime/state (:peer ctx) [::runtime/auto-transact])
        (fragment :_ [:input {:id ::auto-transact :type "checkbox" :checked true
                              :on-click (fn [] (runtime/dispatch! (:peer ctx) [:disable-auto-transact]))}]
                  [:label {:for ::auto-transact} "auto-transact"])
        (fake-managed-anchor :stage [] ctx "stage" :class (if dirty? "stage-dirty")))
      ((:anchor ctx) :new-fiddle [0] ctx "new-fiddle")
      (if (:user-profile ctx)
        ((:anchor ctx) :account [] ctx (get-in ctx [:user-profile :email]))
        (let [auth-state (base64-url-safe/encode (runtime/encode-route (:peer ctx) (:target-route ctx)))]
          [:span.nav-link.auth [:a {:href (str (stateless-login-url ctx) "&state=" auth-state)} "Login"]]))]]))

(defn ^:export qe-picker-control [props ctx]
  (let [enums [:query :entity :blank]
        change! #((:user-with! ctx) (tx/update-entity-attr @(:cell-data ctx) @(:hypercrud.browser/fat-attribute ctx) %))
        options (->> enums
                     (map #(radio/option
                             {:label (case % :query "query" :entity "pull" :blank "blank")
                              :target %
                              :value @(:value ctx)
                              :change! change!})))]
    [:span.qe.radio-group (apply fragment :_ options)]))

(defn ^:export stage-ui [ctx]
  (let [writes-allowed? (or (foundation/alias? (foundation/hostname->hf-domain-name ctx))
                            @(r/fmap (r/partial foundation/domain-owner? (:user-profile ctx))
                                     (runtime/state (:peer ctx) [::runtime/domain])))
        anonymous? (nil? (:user-profile ctx))
        stage @(runtime/state (:peer ctx) [:stage])]
    [:div.hyperfiddle-topnav-stage
     (result/fiddle ctx)                                    ; for docstring
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
     [markdown "Hyperfiddle always generates valid transactions, if it doesn't, please file a bug.

*WARNING:* Datomic schema alterations cannot be used in the same transaction, for now you'll
need to transact the schema before using, see [#6](https://github.com/hyperfiddle/hyperfiddle/issues/6)."]]))
