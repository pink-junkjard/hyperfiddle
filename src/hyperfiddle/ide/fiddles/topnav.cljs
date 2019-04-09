(ns hyperfiddle.ide.fiddles.topnav
  (:require
    [cats.monad.either :as either]
    [contrib.reactive :as r]
    [contrib.ui.tooltip :refer [tooltip]]
    [hyperfiddle.api :as hf]
    [hyperfiddle.data]
    [hyperfiddle.domain :as domain]
    [hyperfiddle.foundation :as foundation]
    [hyperfiddle.ide.domain :as ide-domain]
    [hyperfiddle.runtime :as runtime]
    [hyperfiddle.security.client :as security]
    [hyperfiddle.ui :as ui]
    [hyperfiddle.ui.staging :as staging]))


(defn any-loading? [peer]
  (some (comp not nil? :hydrate-id val) @(runtime/state peer [::runtime/partitions])))

(defn loading-spinner [ctx & [?class]]
  (if @(r/track any-loading? (:peer ctx))
    [:div.display-inline-flex [re-com.core/throbber]]))

(defmethod hf/tx ::new-fiddle-tx #_#{:hyperfiddle.ide/topnav-new
                                   :hyperfiddle.ide/new-fiddle}
  [ctx [e a v] props]
  [[:db/add v :fiddle/type :query]])

(defn topnav-new-wrapper-render [_ ctx props]
  ; iframe wrapper for naked qfind color tag
  [ui/link :hyperfiddle.ide/new-fiddle ctx "new"
   (let [disabled? (not (security/can-create? ctx))   ; we explicitly know the context here is $
         anonymous? (nil? @(runtime/state (:peer ctx) [::runtime/user-id]))]
     {:disabled disabled?
      :tooltip (cond
                 (and anonymous? disabled?) [:warning "Please login"]
                 disabled? [:warning "Writes restricted"])
      :hyperfiddle.ui.popover/redirect (fn [popover-data]
                                         [(:fiddle/ident popover-data)])})])

(defn renderer' [ctx props left-child right-child]
  [:div props
   [:div.left-nav
    [tooltip {:label "Home"} [:a {:href "/"} (:app-domain-ident (runtime/domain (:peer ctx)))]]
    (let [props {:tooltip [nil "Fiddles in this domain"]
                 :iframe-as-popover true}]
      [ui/link :hyperfiddle.ide/entry-point-fiddles ctx "index" props])
    [:span (let [[fiddle-ident :as route] @(runtime/state (:peer ctx) [::runtime/partitions foundation/root-branch :route])]
             (cond
               (= fiddle-ident :hyperfiddle.ide/edit) (let [[_ [user-fiddle-ident]] route]
                                                        (str #_"edit: "
                                                             (if (and (coll? user-fiddle-ident) (= :fiddle/ident (first user-fiddle-ident)))
                                                               (let [user-fiddle-ident (second user-fiddle-ident)]
                                                                 (if (keyword? user-fiddle-ident)
                                                                   (name user-fiddle-ident)
                                                                   user-fiddle-ident))
                                                               user-fiddle-ident)))
               (keyword? fiddle-ident) (name fiddle-ident)
               :else fiddle-ident))]
    left-child]

   [:div.right-nav {:key "right-nav"}                       ; CAREFUL; this key prevents popover flickering
    [loading-spinner ctx]
    right-child
    [hyperfiddle.ui/link :hyperfiddle.ide/topnav-new ctx nil {:user-renderer topnav-new-wrapper-render}]
    [tooltip {:label "Environment administration"} (ui/link :hyperfiddle.ide/env ctx "env")]
    (when (-> (runtime/domain (:peer ctx)) (domain/database "$users"))
      (if @(runtime/state (:peer ctx) [::runtime/user-id])
        (if-let [{:keys [:hypercrud.browser/result]} (hyperfiddle.data/browse ctx :hyperfiddle.ide/account)]
          (let [props {:tooltip [nil @(r/fmap :user/email result)]
                       :iframe-as-popover true}]
            [ui/link :hyperfiddle.ide/account ctx @(r/fmap :user/name result) props]))
        [:a {:href (hyperfiddle.ide/stateless-login-url ctx)} "login"]))]])

(defn hack-login-renderer [ctx props _ _]
  [:div props
   [:div.left-nav
    [tooltip {:label "Home"} [:a (:app-domain-ident (runtime/domain (:peer ctx)))]]]
   [:div.right-nav {:key "right-nav"}                       ; CAREFUL; this key prevents popover flickering
    [loading-spinner ctx]]])

(defn renderer [_ ctx props left-child right-child]
  (let [f (if (= :hyperfiddle.ide/please-login (first @(runtime/state (:peer ctx) [::runtime/partitions foundation/root-branch :route])))
            hack-login-renderer
            renderer')]
    [f ctx props left-child right-child]))
