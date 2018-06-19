(ns hyperfiddle.ide.fiddles.topnav
  (:require [cats.core :refer [fmap]]
            [contrib.data :refer [kwargs unwrap]]
            [contrib.datomic-tx :as tx]
            [contrib.reactive :as r]
            [contrib.reader :refer [read-edn-string]]
            [contrib.reagent :refer [fragment]]
            [contrib.rfc3986 :refer [encode-ednish encode-rfc3986-pchar]]
            [contrib.ui :refer [radio-option]]
            [contrib.ui.tooltip :refer [tooltip]]
            [hypercrud.browser.context :as context]
            [hypercrud.browser.fiddle :as fiddle]
            [hypercrud.browser.link :as link]
            [hypercrud.browser.router :as router]
            [hypercrud.browser.system-fiddle :as system-fiddle]
            [hypercrud.types.Entity :refer [->Entity shadow-entity]]
            [hyperfiddle.actions :as actions]
            [hyperfiddle.foundation :as foundation]
            [hyperfiddle.runtime :as runtime]
            [hyperfiddle.security :as security]
            [hyperfiddle.ui :as ui]))


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
        {:keys [hypercrud.browser/result
                hypercrud.browser/fiddle] :as ctx} (shadow-fiddle ctx)
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
      [tooltip {:label "Home"} [:a.hf-auto-nav {:href "/"} @(runtime/state (:peer ctx) [::runtime/domain :domain/ident])]]
      [tooltip {:label "This fiddle"}                       ; also a good place for the route
       [:span.hf-auto-nav (some-> @(r/cursor (:hypercrud.browser/result ctx) [:fiddle/ident]) str)]
       #_[tooltip {:label nil} (fake-managed-anchor :shortcuts [] ctx "shortcuts")]]]

     [:div.right-nav {:key "right-nav"}                     ; CAREFUL; this key prevents popover flickering

      [loading-spinner ctx]

      (let [src-mode (src-mode? (-> ctx :target-route (get 3)))
            no-target-fiddle (nil? (:db/id @result))        ; ide-route omits fiddle for ide routes
            change! #(runtime/dispatch! (:peer ctx) (actions/set-display-mode %))
            value (if src-mode :src display-mode)]
        [:span.radio-group
         (radio-option {:label "data" :tooltip "Ignore :fiddle/renderer" :target :hypercrud.browser.browser-ui/xray :change! change! :value value
                        :disabled (or src-mode no-target-fiddle)})
         (radio-option {:label "view" :tooltip "Use :fiddle/renderer" :target :hypercrud.browser.browser-ui/user :value value :change! change!
                        :disabled (or src-mode no-target-fiddle)})
         (radio-option {:label (let [root-rel-path (runtime/encode-route (:peer ctx) (router/dissoc-frag (:target-route ctx)))
                                     href (if-not src-mode
                                            (str root-rel-path "#" (encode-rfc3986-pchar (encode-ednish (pr-str :src))))
                                            (str root-rel-path "#"))]
                                 (if (and (not src-mode) (not no-target-fiddle))
                                   [:a {:href href :target "_blank"} "src"]
                                   [:span "src"]))
                        :tooltip "View fiddle source" :target :hypercrud.browser.browser-ui/src :value value :change! change!
                        :disabled (or (not src-mode) no-target-fiddle)})])
      (let [dirty? (not @(r/fmap empty? (runtime/state (:peer ctx) [:stage nil])))]
        (fake-managed-anchor :stage [] ctx "stage" :class (when dirty? "stage-dirty")))
      (ui/link :new-fiddle [] ctx "new-fiddle" #_#_:tooltip (if-not (writes-allowed? ctx) [:warning "Domain owners only"]))
      [tooltip {:label "Domain administration"} (ui/link :domain [] ctx "domain")]
      (if @(runtime/state (:peer ctx) [::runtime/user-id])
        [ui/browse :account [] ctx]
        [:span.nav-link.auth [:a {:href (foundation/stateless-login-url ctx)} "Login"]])]]))

(defn ^:export qe-picker-control [value ctx props]
  (let [enums [:query :entity :blank]
        entity (context/entity ctx)
        change! #((:user-with! ctx) (tx/update-entity-attr @entity @(:hypercrud.browser/fat-attribute ctx) %))
        options (->> enums
                     (map #(radio-option
                             {:label (case % :query "query" :entity "pull" :blank "blank")
                              :target %
                              :value value
                              :change! change!})))]
    [:span.qe.radio-group (apply fragment :_ options)]))

(defn stage-ui-buttons [selected-uri stage ctx]
  (let [writes-allowed? (let [hf-db @(runtime/state (:peer ctx) [::runtime/domain :domain/db-lookup @selected-uri])
                              subject @(runtime/state (:peer ctx) [::runtime/user-id])]
                          (security/attempt-to-transact? hf-db subject))
        anonymous? (nil? @(runtime/state (:peer ctx) [::runtime/user-id]))]
    (fragment
      :_
      (let [disabled? (or (not writes-allowed?) (not (empty? @stage)))]
        [tooltip (cond (and anonymous? (not writes-allowed?)) {:status :warning :label "Please login"}
                       (not writes-allowed?) {:status :warning :label "Writes restricted"}
                       (not (empty? @stage)) {:status :warning :label "please transact! all changes first"})
         [:button {:disabled disabled?
                   :style (if disabled? {:pointer-events "none"})
                   :on-click (fn [] (runtime/dispatch! (:peer ctx) [:toggle-auto-transact @selected-uri]))}
          (str (if @(runtime/state (:peer ctx) [::runtime/auto-transact @selected-uri]) "Disable" "Enable") " auto-transact")]])
      (let [disabled? (or (not writes-allowed?) (empty? @stage))]
        [tooltip (cond (and (not writes-allowed?) anonymous?) {:status :warning :label "Please login"}
                       (not writes-allowed?) {:status :warning :label "Writes restricted"}
                       (empty? @stage) {:status :warning :label "no changes"})
         [:button {:disabled disabled?
                   :style (if disabled? {:pointer-events "none"})
                   :on-click (fn []
                               (let [invert-route (:hypercrud.browser/invert-route ctx)
                                     ; specifically dont use the SplitRuntime protocol here. the only thing that makes sense is whats in the context from the route
                                     nil-branch-aux {:hyperfiddle.ide/foo "page"}
                                     action (actions/manual-transact-uri! (:peer ctx) invert-route nil-branch-aux @selected-uri)]
                                 (runtime/dispatch! (:peer ctx) action)))}
          "transact!"]]))))

(defn ^:export stage-ui [ctx]
  [:div.hyperfiddle-topnav-stage
   (ui/fiddle ctx)                                          ; for docstring
   [foundation/staging ctx stage-ui-buttons]])
