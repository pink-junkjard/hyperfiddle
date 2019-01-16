(ns hyperfiddle.ide.fiddles.topnav
  (:require
    [cats.monad.either :as either]
    [clojure.string :as string]
    [contrib.reactive :as r]
    [contrib.ui :refer [easy-checkbox]]
    [contrib.ui.tooltip :refer [tooltip]]
    [hyperfiddle.actions :as actions]
    [hyperfiddle.data]
    [hyperfiddle.domain :as domain]
    [hyperfiddle.runtime :as runtime]
    [hyperfiddle.security.client :as security]
    [hyperfiddle.ui :as ui :refer [markdown]]))


(defn any-loading? [peer]
  (some (comp not nil? :hydrate-id val) @(runtime/state peer [::runtime/partitions])))

(defn loading-spinner [ctx & [?class]]
  (if @(r/track any-loading? (:peer ctx))
    [:div.display-inline-flex [re-com.core/throbber]]))

(defn renderer' [val ctx props]
  [:div props
   [:div.left-nav
    [tooltip {:label "Home"} [:a {:href "/"} (domain/ident (runtime/domain (:peer ctx)))]]
    [:span (let [[fiddle-ident :as route] @(runtime/state (:peer ctx) [::runtime/partitions nil :route])]
             (cond
               (= fiddle-ident :hyperfiddle.ide/edit) (let [[_ [user-fiddle-ident]] route]
                                                        (str "Editing: "
                                                             (if (keyword? user-fiddle-ident)
                                                               (name user-fiddle-ident)
                                                               user-fiddle-ident)))
               (keyword? fiddle-ident) (name fiddle-ident)
               :else fiddle-ident))]
    (let [props {:tooltip [nil "Fiddles in this domain"]
                 :iframe-as-popover true}]
      [ui/link :fiddle-shortcuts ctx "index" props])]

   [:div.right-nav {:key "right-nav"}                       ; CAREFUL; this key prevents popover flickering
    [loading-spinner ctx]
    (let [tooltip [:div {:style {:text-align "left"}}
                   [markdown
                    (->> (runtime/domain (:peer ctx))
                         domain/databases
                         keys
                         sort
                         (map (fn [dbname]
                                (let [prefix (if @(runtime/state (:peer ctx) [::runtime/auto-transact dbname])
                                               "- [x] "
                                               "- [ ] ")]
                                  (str prefix dbname))))
                         (string/join "\n")
                         (str "##### Auto-transact:\n\n"))
                    {:hyperfiddle.ui.markdown-extensions/unp true}]]
          props {:tooltip [nil tooltip]
                 :class (when-not @(r/fmap empty? (runtime/state (:peer ctx) [::runtime/partitions nil :stage]))
                          "stage-dirty")
                 :iframe-as-popover true}]
      [ui/link :stage ctx "stage" props])
    (ui/link :new-fiddle ctx "new" (let [disabled? (not (security/can-create? ctx)) ; we explicitly know the context here is $
                                         anonymous? (nil? @(runtime/state (:peer ctx) [::runtime/user-id]))]
                                     {:disabled disabled?
                                      :tooltip (cond
                                                 (and anonymous? disabled?) [:warning "Please login"]
                                                 disabled? [:warning "Writes restricted"])
                                      :hyperfiddle.ui.popover/redirect (fn [popover-data]
                                                                         [(:fiddle/ident popover-data)])}))
    [tooltip {:label "Environment administration"} (ui/link :domain ctx "env")]
    (if @(runtime/state (:peer ctx) [::runtime/user-id])
      (if-let [{:keys [:hypercrud.browser/data]} (hyperfiddle.data/browse ctx :account)]
        (let [props {:tooltip [nil @(r/fmap :user/email data)]
                     :iframe-as-popover true}]
          [ui/link :account ctx @(r/fmap :user/name data) props]))
      [:a {:href (hyperfiddle.ide/stateless-login-url ctx)} "login"])]])

(defn hack-login-renderer [val ctx props]
  [:div props
   [:div.left-nav
    [tooltip {:label "Home"} [:a (domain/ident (runtime/domain (:peer ctx)))]]]
   [:div.right-nav {:key "right-nav"}                       ; CAREFUL; this key prevents popover flickering
    [loading-spinner ctx]]])

(defn renderer [val ctx props]
  (let [f (if (= :hyperfiddle.ide/please-login (first @(runtime/state (:peer ctx) [::runtime/partitions nil :route])))
            hack-login-renderer
            renderer')]
    [f val ctx props]))

(letfn [(toggle-auto-transact! [ctx selected-dbname]
          (runtime/dispatch! (:peer ctx) [:toggle-auto-transact @selected-dbname]))]
  (defn ^:export stage-ui-buttons [selected-dbname stage ctx]
    (let [writes-allowed?+ (let [hf-db (domain/database (runtime/domain (:peer ctx)) @selected-dbname)
                                 subject @(runtime/state (:peer ctx) [::runtime/user-id])]
                             (security/subject-can-transact? hf-db subject))
          anonymous? (nil? @(runtime/state (:peer ctx) [::runtime/user-id]))]
      [:<>
       [tooltip (either/branch
                  writes-allowed?+
                  (fn [e] {:status :warning :label "Misconfigured db security"})
                  (fn [writes-allowed?]
                    (cond (and (not writes-allowed?) anonymous?) {:status :warning :label "Please login"}
                          (not writes-allowed?) {:status :warning :label "Writes restricted"}
                          (empty? @stage) {:status :warning :label "no changes"})))
        (let [disabled? (either/branch
                          writes-allowed?+
                          (constantly true)
                          (fn [writes-allowed?] (or (not writes-allowed?) (empty? @stage))))]
          [:button {:disabled disabled?
                    :style (if disabled? {:pointer-events "none"})
                    :on-click (fn []
                                (let [action (actions/manual-transact-db! (:peer ctx) @selected-dbname)]
                                  (runtime/dispatch! (:peer ctx) action)))}
           "transact!"])]
       " "
       #_[tooltip (either/branch
                    writes-allowed?+
                    (fn [e] {:status :warning :label "Misconfigured db security"})
                    (fn [writes-allowed?]
                      (cond (and anonymous? (not writes-allowed?)) {:status :warning :label "Please login"}
                            (not writes-allowed?) {:status :warning :label "Writes restricted"}
                            (not (empty? @stage)) {:status :warning :label "please transact! all changes first"})))
          (let [is-disabled (either/branch
                              writes-allowed?+
                              (constantly true)
                              (fn [writes-allowed?]
                                (or (not writes-allowed?) (not (empty? @stage)))))
                is-auto-transact @(runtime/state (:peer ctx) [::runtime/auto-transact @selected-dbname])]
            [easy-checkbox {:disabled is-disabled
                            :style (if is-disabled {:pointer-events "none"})
                            :checked (boolean is-auto-transact)
                            :on-change (r/partial toggle-auto-transact! ctx selected-dbname)}
             "auto-transact"])]])))
